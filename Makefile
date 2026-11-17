
.SUFFIXES:

ifeq ($(strip $(DEVKITPRO)),)
$(error "DEVKITPRO not set - open devkitPro MSYS2")
endif

TOPDIR ?= $(CURDIR)
include $(DEVKITPRO)/libnx/switch_rules

TARGET   := RadioNX
BUILD    := build
SOURCES  := src
ROMFS    := assets
INCLUDES :=

APP_TITLE   := RadioNX
APP_AUTHOR  := Muveeer
APP_VERSION := 1.0.0
ICON        := icon.jpg

export APP_TITLE   := RadioNX
export APP_AUTHOR  := Muveeer
export APP_VERSION := 1.0.0
export APP_ICON    := $(TOPDIR)/icon.jpg

ARCH := -march=armv8-a+crc+crypto -mtune=cortex-a57 -mtp=soft -fPIE

CFLAGS := -g -O2 -Wall -Wno-misleading-indentation -Wno-format-truncation \
          -ffunction-sections -fdata-sections $(ARCH)
CFLAGS += $(INCLUDE) -D__SWITCH__ -I$(DEVKITPRO)/portlibs/switch/include/SDL2

CXXFLAGS := $(CFLAGS) -std=gnu++17 -fexceptions -frtti
ASFLAGS  := -g $(ARCH)
LDFLAGS  = -specs=$(DEVKITPRO)/libnx/switch.specs -g $(ARCH) -Wl,-Map,$(notdir $*.map)

LIBDIRS := $(DEVKITPRO)/portlibs/switch $(DEVKITPRO)/libnx

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
                   $(foreach dir,$(LIBDIRS),-I$(dir)/include) \
                   -I$(CURDIR)/$(BUILD)
export LIBPATHS := $(foreach dir,$(LIBDIRS),-L$(dir)/lib)

icons := $(wildcard *.jpg)
ifneq (,$(findstring $(TARGET).jpg,$(icons)))
	export APP_ICON := $(TOPDIR)/$(TARGET).jpg
else
	ifneq (,$(findstring icon.jpg,$(icons)))
		export APP_ICON := $(TOPDIR)/icon.jpg
	endif
endif

ifneq ($(ROMFS),)
	export NROFLAGS += --romfsdir=$(CURDIR)/$(ROMFS)
endif

.PHONY: all clean $(BUILD)

all: $(BUILD)

$(BUILD):
	@[ -d $@ ] || mkdir -p $@
	@$(MAKE) --no-print-directory -C $(BUILD) -f $(CURDIR)/Makefile

clean:
	@echo clean ...
	@rm -fr $(BUILD) $(TARGET).nro $(TARGET).nacp $(TARGET).elf

else

DEPENDS := $(OFILES:.o=.d)

all: $(OUTPUT).nro

$(OUTPUT).nacp:
	@echo creating ... $(notdir $@)
	nacptool --create "RadioNX" "Muveeer" "1.0.0" $@

$(OUTPUT).nro: $(OUTPUT).elf $(OUTPUT).nacp
	@echo built ... $(notdir $@)
	elf2nro $< $@ --icon=$(TOPDIR)/icon.jpg --nacp=$(OUTPUT).nacp $(NROFLAGS)

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
