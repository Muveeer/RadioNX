cmake_minimum_required(VERSION 3.16)
project(RadioSphereNX CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()
find_package(PkgConfig REQUIRED)
find_package(CURL REQUIRED)
pkg_check_modules(SDL2 REQUIRED sdl2)
pkg_check_modules(SDL2_TTF REQUIRED SDL2_ttf)
pkg_check_modules(FFMPEG REQUIRED libavformat libavcodec libswresample libswscale libavutil)
add_executable(RadioSphereNX src/main.cpp)
target_include_directories(RadioSphereNX PRIVATE
  ${SDL2_INCLUDE_DIRS} ${SDL2_TTF_INCLUDE_DIRS} ${FFMPEG_INCLUDE_DIRS} ${CURL_INCLUDE_DIRS})
target_link_libraries(RadioSphereNX PRIVATE
  ${SDL2_LIBRARIES} ${SDL2_TTF_LIBRARIES} ${FFMPEG_LIBRARIES} ${CURL_LIBRARIES})

set(ASSETS ${CMAKE_SOURCE_DIR}/assets)
file(MAKE_DIRECTORY ${ASSETS})
set(FONTS
  "font.ttf|https://raw.githubusercontent.com/google/fonts/main/ofl/nunito/Nunito%5Bwght%5D.ttf"
  "font_cjk.ttf|https://raw.githubusercontent.com/google/fonts/main/ofl/notosansjp/NotoSansJP%5Bwght%5D.ttf"
  "font_ar.ttf|https://raw.githubusercontent.com/google/fonts/main/ofl/notosansarabic/NotoSansArabic%5Bwght%5D.ttf"
  "font_he.ttf|https://raw.githubusercontent.com/google/fonts/main/ofl/notosanshebrew/NotoSansHebrew%5Bwght%5D.ttf"
  "font_th.ttf|https://raw.githubusercontent.com/google/fonts/main/ofl/notosansthai/NotoSansThai%5Bwght%5D.ttf"
  "font_hi.ttf|https://raw.githubusercontent.com/google/fonts/main/ofl/notosansdevanagari/NotoSansDevanagari%5Bwght%5D.ttf")
foreach(F ${FONTS})
  string(REPLACE "|" ";" PARTS ${F})
  list(GET PARTS 0 NAME)
  list(GET PARTS 1 URL)
  if(NOT EXISTS ${ASSETS}/${NAME})
    message(STATUS "Downloading ${NAME} ...")
    file(DOWNLOAD ${URL} ${ASSETS}/${NAME} STATUS ST TIMEOUT 180)
    list(GET ST 0 CODE)
    if(NOT CODE EQUAL 0)
      file(REMOVE ${ASSETS}/${NAME})
      message(WARNING "${NAME} failed to download (some scripts may show boxes)")
    endif()
  endif()
endforeach()
if(NOT EXISTS ${ASSETS}/cacert.pem)
  file(DOWNLOAD "https://curl.se/ca/cacert.pem" ${ASSETS}/cacert.pem STATUS ST2)
  list(GET ST2 0 C2)
  if(NOT C2 EQUAL 0)
    file(REMOVE ${ASSETS}/cacert.pem)
  endif()
endif()
add_custom_command(TARGET RadioSphereNX POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy_directory ${ASSETS} $<TARGET_FILE_DIR:RadioSphereNX>/assets)