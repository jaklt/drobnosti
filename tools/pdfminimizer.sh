#!/bin/bash
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite \
  -dCompatibilityLevel=1.4             \
  -dEmbedAllFonts=true                 \
  -dCannotEmbedFontPolicy=/Error       \
  -dPreserveOPIComments=true           \
  -dColorImageResolution=150           \
  -dGrayImageResolution=300            \
  -dMonoImageResolution=1200           \
  -dPreserveOverprintSettings=true     \
  -dCreateJobTicket=true               \
  -dAutoRotatePages=/None              \
  -dUCRandBGInfo=/Preserve             \
  -dDoThumbnails=true                  \
  -dPreserveEPSInfo=true               \
                                       \
  -dMonoImageDownsampleType=/Average   \
  -dOptimize=true                      \
  -dDownsampleColorImages=true         \
  -dDownsampleGrayImages=true          \
  -dDownsampleMonoImages=true          \
  -dUseCIEColor                        \
  -dColorConversionStrategy=/LeaveColorUnchanged \
  -sOutputFile=output.pdf $1 \
&& du -h output.pdf $1

# -dNeverEmbed=[]  \
# -dColorImageDownsampleType=/Bicubic \
# -dMonoImageDownsampleType=/Bicubic  \
# -dFIXEDMEDIA \

# http://ghostscript.com/doc/current/Ps2pdf.htm
# http://stackoverflow.com/questions/11850776/reducing-pdf-file-size-using-ghostscript-on-linux-didnt-work

# Parameters taken from (prepress):
# gs -dNODISPLAY -c ".distillersettings {exch ==only ( ) print ===} forall quit" | less

