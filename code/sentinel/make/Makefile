#
# Makefile
#
# Version:
#     $Id: Makefile,v 1.4 2010/06/21 20:56:52 kyledewey Exp kyledewey $
#
# Revisions:
#      $Log: Makefile,v $
#      Revision 1.4  2010/06/21 20:56:52  kyledewey
#      Now uses fsc instead of scalac for compilation;
#      compilation time improved.
#
#      Revision 1.3  2010/06/18 20:56:38  kyledewey
#      Fitted with customized scaladoc.
#
#      Revision 1.2  2010/06/18 03:12:26  kyledewey
#      Removed -k option from each call to make.
#
#      Revision 1.1  2010/06/16 00:05:12  kyledewey
#      Initial revision
#
#
#

# structure borrowed from:
# http://www.mail-archive.com/ant-dev@jakarta.apache.org/msg03644.html

# Directories
CLASS_DIR = $(JAVA_DEV_ROOT)/classes
DOC_DIR = $(JAVA_DEV_ROOT)/docs
MAKE_DIR = $(JAVA_DEV_ROOT)/make
SOURCE_DIR = $(JAVA_DEV_ROOT)/src

# Tools and options
PRINT = @echo
SCALA = scala
SCALAC = fsc
DELETE = rm -f
COPY = cp
SCALADOC = scaladoc
SCALAC_OPTIONS = -d $(CLASS_DIR) -unchecked -explaintypes -deprecation
SCALADOC_OPTIONS = \
	-d $(DOC_DIR) \
	-sourcepath $(SOURCE_DIR) \
	-windowtitle $(WINDOWTITLE) \
	-doctitle $(DOCTITLE)

# Rules and dependencies
PACKAGE_LOC = $(subst .,/,$(PACKAGE))
PACKAGE_DIR = $(CLASS_DIR)/$(PACKAGE_LOC)
SCALA_FILES = $(filter %.scala,$(SOURCE))
CLASS_FILES = $(SCALA_FILES:%.scala=$(PACKAGE_DIR)/%.class)

ifdef PACKAGE
PACKAGE_LIST = $(subst .,/,$(PACKAGE))
else
PACKAGE_LIST = $(subst .,/,$(PACKAGES)) 
endif

PLIST_CLEAN = $(patsubst %,$(SOURCE_DIR)/%/.clean,$(PACKAGE_LIST))
PLIST_BUILD = $(patsubst %,$(SOURCE_DIR)/%/.build,$(PACKAGE_LIST))

# Rule 0
default: buildall

# Rule 1: Build a .class from a .scala file
$(PACKAGE_DIR)/%.class :: $(SOURCE_DIR)/$(PACKAGE_LOC)/%.scala
	$(SCALAC) $(SCALAC_OPTIONS) $< 

# Rule 2: Build a .class from a .scala.  Uses Rule 1
%.class : $(SOURCE_DIR)/$(PACKAGE_LOC)/%.scala
	$(MAKE) $(PACKAGE_DIR)/$@

# ...Rules from source that have no relevance to me...

# Rule 10: Build class files
all: $(CLASS_FILES)

# ...more rules with no relevance...

# Rule 13: cleanup
cleanall:
	$(DELETE) $(PACKAGE_DIR)/*.class

# Rule 14: recall this makefile with target cleanall
%.clean:
	$(MAKE) -f $(subst .clean,Makefile,$@) cleanall

# Rule 15: call 14 for every package dir
clean: $(PLIST_CLEAN)
	$(PRINT) Done clean.

# Rule 16: Change .build tag to Makefile; allows for recursive build
%.build:
	$(MAKE) -f $(subst .build,Makefile,$@) all

# Rule 17: Call rule 16 for each package
buildall: $(PLIST_BUILD)
	$(PRINT) Done build.

# ..more rules with no relevance...

# Rule 21: Build scaladoc for all listed packages
# note that scaladoc is not as robust as javadoc, so neither works
#scaladoc:
#	$(PRINT) $(PACKAGES) > $(JAVA_DEV_ROOT)/packages.tmp
#	$(SCALADOC) $(SCALADOC_OPTIONS) @$(JAVA_DEV_ROOT)/packages.tmp
#	$(DELETE) $(JAVA_DEV_ROOT)/packages.tmp
#	$(PRINT) Done Scaladoc.
#	find src/ -name '*.scala' | xargs $(SCALADOC) $(SCALADOC_OPTIONS)
# instead, use a customized scaladoc
scaladoc:
	perl scaladoc.pl
	$(PRINT) Done Scaladoc.

