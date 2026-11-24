.SUFFIXES:
.DEFAULT_GOAL := all

ifeq ($(strip $(DEVKITPRO)),)
$(error "DEVKITPRO not set - open devkitPro MSYS2")
endif

TOPDIR ?= $(CURDIR)
include $(DEVKITPRO)/libnx/switch_rules

TARGET   := RadioNX
BUILD    := build
SOURCES  := src
ROMFS    := romfs
ASSETS   := assets
INCLUDES :=

APP_TITLE   := RadioNX
APP_AUTHOR  := Muveeer
APP_VERSION := 2.2.0

export APP_TITLE APP_AUTHOR APP_VERSION

# 1 = класть font_cjk.ttf в NRO (иероглифы на штампах), 0 = минус ~9 МБ
EMBED_CJK ?= 1

ARCH := -march=armv8-a+crc+crypto -mtune=cortex-a57 -mtp=soft -fPIE

CFLAGS := -g -O2 -Wall -Wno-misleading-indentation -Wno-format-truncation \
          -ffunction-sections -fdata-sections $(ARCH)
CFLAGS += $(INCLUDE) -D__SWITCH__ -I$(DEVKITPRO)/portlibs/switch/include/SDL2

CXXFLAGS := $(CFLAGS) -std=gnu++17 -fexceptions -frtti
ASFLAGS  := -g $(ARCH)
LDFLAGS  = -specs=$(DEVKITPRO)/libnx/switch.specs -g $(ARCH) \
           -Wl,--gc-sections -Wl,-Map,$(notdir $*.map)

LIBDIRS := $(DEVKITPRO)/portlibs/switch $(DEVKITPRO)/libnx

ROMFS_ABS := $(CURDIR)/$(ROMFS)
GTA_SRC   := $(CURDIR)/$(ASSETS)/gta

ifneq ($(BUILD),$(notdir $(CURDIR)))

export OUTPUT  := $(CURDIR)/$(TARGET)
export TOPDIR  := $(CURDIR)
export VPATH   := $(foreach dir,$(SOURCES),$(CURDIR)/$(dir))
export DEPSDIR := $(CURDIR)/$(BUILD)

CFILES   := $(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.c)))
CPPFILES := $(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.cpp)))

export LD := $(CXX)

export OFILES   := $(CPPFILES:.cpp=.o) $(CFILES:.c=.o)
export INCLUDE  := $(foreach dir,$(INCLUDES),-I$(CURDIR)/$(dir)) \
                   $(foreach dir,$(SOURCES),-I$(CURDIR)/$(dir)) \
                   $(foreach dir,$(LIBDIRS),-I$(dir)/include) \
                   -I$(CURDIR)/$(BUILD)
export LIBPATHS := $(foreach dir,$(LIBDIRS),-L$(dir)/lib)

ICON_FILE := $(firstword $(wildcard $(CURDIR)/$(TARGET).jpg $(CURDIR)/icon.jpg $(CURDIR)/$(ASSETS)/icon.jpg))

ifneq ($(ICON_FILE),)
export APP_ICON := $(ICON_FILE)
export NROFLAGS += --icon=$(ICON_FILE)
endif

ifneq ($(ROMFS),)
export NROFLAGS += --romfsdir=$(ROMFS_ABS)
endif

.PHONY: all clean $(BUILD) assets

all: $(BUILD)

$(BUILD): assets
	@[ -d $@ ] || mkdir -p $@
	@$(MAKE) --no-print-directory -C $(BUILD) -f $(CURDIR)/Makefile

# БЕЗ СТИРАНИЯ: romfs — рабочая папка, assets только доливает сверху.
# Иконки можно держать прямо в romfs/gta/stations — make их не удалит.
assets:
	@echo "romfs: sync (no wipe) ..."
	@test -f "$(CURDIR)/$(ASSETS)/font.ttf" || { echo "ERROR: assets/font.ttf not found"; exit 1; }
	@mkdir -p "$(ROMFS_ABS)/assets" "$(ROMFS_ABS)/gta/sfx" "$(ROMFS_ABS)/gta/stations" "$(ROMFS_ABS)/gta/games"
	@cp -f "$(CURDIR)/$(ASSETS)/font.ttf" "$(ROMFS_ABS)/assets/"
ifeq ($(EMBED_CJK),1)
	@if [ -f "$(CURDIR)/$(ASSETS)/font_cjk.ttf" ]; then cp -f "$(CURDIR)/$(ASSETS)/font_cjk.ttf" "$(ROMFS_ABS)/assets/"; fi
endif
	@if [ -f "$(CURDIR)/$(ASSETS)/cacert.pem" ]; then cp -f "$(CURDIR)/$(ASSETS)/cacert.pem" "$(ROMFS_ABS)/assets/"; fi
	@if [ -f "$(GTA_SRC)/catalog.json" ]; then cp -f "$(GTA_SRC)/catalog.json" "$(ROMFS_ABS)/gta/"; fi
	@test -f "$(ROMFS_ABS)/gta/catalog.json" || { echo "ERROR: gta/catalog.json not found (assets/gta or romfs/gta)"; exit 1; }
	@if [ -d "$(GTA_SRC)/stations" ]; then cp -rf "$(GTA_SRC)/stations/." "$(ROMFS_ABS)/gta/stations/"; fi
	@if [ -d "$(GTA_SRC)/games" ];    then cp -rf "$(GTA_SRC)/games/."    "$(ROMFS_ABS)/gta/games/";    fi
	@for f in change-station-sa.mp3 change-station-vc.mp3; do \
		if [ -f "$(GTA_SRC)/sfx/$$f" ]; then cp -f "$(GTA_SRC)/sfx/$$f" "$(ROMFS_ABS)/gta/sfx/"; fi; \
	done
	@find "$(ROMFS_ABS)" -type d -name '_backup*' -prune -exec rm -rf {} + 2>/dev/null || true
	@find "$(ROMFS_ABS)" -type d -name '.git' -prune -exec rm -rf {} + 2>/dev/null || true
	@find "$(ROMFS_ABS)" -type f \( -name 'Thumbs.db' -o -name 'desktop.ini' -o -name '.DS_Store' \) -delete 2>/dev/null || true
	@echo "romfs breakdown:"
	@du -sh "$(ROMFS_ABS)/assets" "$(ROMFS_ABS)/gta" 2>/dev/null || true
	@du -sh "$(ROMFS_ABS)" 2>/dev/null || true

# romfs НЕ удаляем — там рабочие ассеты (иконки GTA VI и т.д.)
clean:
	@echo clean ...
	@rm -fr $(BUILD) $(TARGET).nro $(TARGET).nacp $(TARGET).elf $(TARGET).map

else

DEPENDS := $(OFILES:.o=.d)

all: $(OUTPUT).nro

$(OUTPUT).nacp:
	@echo creating ... $(notdir $@)
	@nacptool --create "$(APP_TITLE)" "$(APP_AUTHOR)" "$(APP_VERSION)" $@

ROMFS_FILES := $(shell find $(TOPDIR)/$(ROMFS) -type f 2>/dev/null)

$(OUTPUT).nro: $(OUTPUT).elf $(OUTPUT).nacp $(ROMFS_FILES)
	@echo built ... $(notdir $@)
	@elf2nro $< $@ --nacp=$(OUTPUT).nacp $(NROFLAGS)

$(OUTPUT).elf: $(OFILES)

%.o: %.cpp
	@echo $(notdir $<)
	@$(CXX) -MMD -MP -MF $(DEPSDIR)/$*.d $(CXXFLAGS) -c $< -o $@

%.o: %.c
	@echo $(notdir $<)
	@$(CC) -MMD -MP -MF $(DEPSDIR)/$*.d $(CFLAGS) -c $< -o $@

-include $(DEPENDS)

endif

LIBS := -Wl,--start-group $(wildcard $(DEVKITPRO)/portlibs/switch/lib/*.a) -Wl,--end-group -lnx