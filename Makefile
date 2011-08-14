
MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

#UNITS=  topsort tigerabs tigertab tigernlin tigertips tigersres tigerpp tigerescap tigergrm tigerlex tigertemp tigertree tigertranslate tigerseman tigermain tigerframe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo
 
all: tigerliv.uo tigercoloring.uo tigerset.uo tigermap.uo tigerintmap.uo tigerliveness.uo tigerassem.uo tigerutils.uo tigercanon.uo tigerstack.uo tigermain.uo tigerabs.uo tigernlin.uo tigertips.uo tigertab.uo tigersres.uo tigerpp.uo tigerescap.uo tigergrm.uo tigerlex.uo tigerseman.uo tigertree.uo tigertemp.uo tigertranslate.uo tigerframe.uo tc 


tc: tigerliv.uo tigercoloring.uo tigerset.uo tigermap.uo tigerintmap.uo tigerliveness.uo tigerassem.uo tigerutils.uo tigercanon.uo tigerstack.uo tigermain.uo tigerabs.uo tigernlin.uo tigertips.uo tigertab.uo tigersres.uo tigerpp.uo tigerescap.uo tigergrm.uo tigerlex.uo tigerseman.uo tigertree.uo tigertemp.uo tigertranslate.uo tigerframe.uo
	$(MOSMLL) -toplevel -o tc tigermain.uo

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f tc

# these rules are only needed if UNITS is undefined or empty
.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep $(UNITS) >> Makefile

### DO NOT DELETE THIS LINE
tigerliv.uo: tigermap.ui tigerassem.ui tigertemp.ui tigerutils.uo \
    tigerset.ui 
tigerset.uo: tigerset.ui 
tigertree.uo: tigertemp.ui tigerutils.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerintmap.uo: tigerintmap.ui 
tigercoloring-old.uo: tigermap.ui tigerframe.ui tigerassem.ui tigerstack.ui \
    tigertemp.ui tigerutils.uo tigerset.ui 
tigerstack.uo: tigerstack.ui 
tigerliveness.uo: tigerliveness.ui tigerassem.ui tigertemp.ui tigerutils.uo \
    tigerintmap.ui 
tigerliveness.ui: tigerassem.ui tigerintmap.ui 
tigertranslate.uo: tigertree.uo tigerframe.ui tigerit.uo tigerstack.ui \
    tigertemp.ui tigerabs.uo tigerutils.uo 
tigerpp.uo: tigerabs.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertranslate.uo tigerabs.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigergrm.ui: tigerabs.uo 
tigerassem.uo: tigerassem.ui tigertree.uo tigerframe.ui tigercanon.ui \
    tigertemp.ui tigerutils.uo 
tigerassem.ui: tigertree.uo tigerframe.ui tigercanon.ui tigertemp.ui 
tigerit.uo: tigertree.uo tigertab.ui tigertemp.ui 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigertips.uo \
    topsort.uo tigertranslate.uo tigerstack.ui tigerabs.uo tigerutils.uo 
tigerseman.ui: tigertranslate.uo tigerabs.uo 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigertranslate.uo \
    tigercanon.ui tigerassem.ui tigerlex.uo tigercoloring.uo \
    tigerliveness.ui tigerpp.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigertemp.uo: tigertemp.ui 
tigerframe.uo: tigerframe.ui tigertree.uo tigertemp.ui tigerutils.uo 
tigerframe.ui: tigertree.uo tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigerframe.ui \
    tigerit.uo tigertemp.ui 
tigercanon.ui: tigertree.uo tigerframe.ui tigertranslate.uo tigertemp.ui 
tigertab.uo: tigertab.ui 
tigermap.uo: tigermap.ui 
tigercoloring.uo: tigermap.ui tigerframe.ui tigerassem.ui tigerstack.ui \
    tigertemp.ui tigerutils.uo tigerset.ui 
