#
# Makefile
#
# Version:
#     $Id: Makefile,v 1.3 2010/06/23 03:16:32 kyledewey Exp kyledewey $
#
# Revisions:
#      $Log: Makefile,v $
#      Revision 1.3  2010/06/23 03:16:32  kyledewey
#      Added sentinel.utils and
#      sentinel.utils.builtins.
#
#      Revision 1.2  2010/06/18 03:11:50  kyledewey
#      Added packages sentinel.model.writer and
#      sentinel.model.writer.xml.
#
#      Revision 1.1  2010/06/16 00:04:35  kyledewey
#      Initial revision
#
#
#

# structure borrowed from:
# http://www.mail-archive.com/ant-dev@jakarta.apache.org/msg03644.html

PACKAGES = \
	sentinel.model \
	sentinel.model.matcher \
	sentinel.model.replacer \
	sentinel.model.parser \
	sentinel.model.parser.xml \
	sentinel.model.writer \
	sentinel.model.writer.xml \
	sentinel.view \
	sentinel.io \
	sentinel.io.input \
	sentinel.io.input.csv \
	sentinel.io.output \
	sentinel.io.output.csv \
	sentinel.project \
	sentinel.utils \
	sentinel.utils.legacy \
	sentinel.utils.builtins \
	sentinel.utils.noninteractive \
	sentinel.utils.interactive \
	sentinel.vpl \

MAIN_CLASS = Factory
MAIN_PACKAGE = sentinel.model

# Javadoc stuff
WINDOWTITLE = 'Error Sentinel API Specification'
DOCTITLE = 'Sentinel API'
HEADER = 'Sentinel API'

include $(JAVA_DEV_ROOT)/make/Makefile
