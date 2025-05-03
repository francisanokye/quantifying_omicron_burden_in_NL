## This is [project.Makefile] …

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

### Makestuff

Sources += Makefile
Sources += $(wildcard *.R)

params.Rout: params.R
	$(pipeR)

keydates.Rout: keydates.R 
	$(pipeR)

flows.Rout: flows.R params.rda
	$(pipeR)

spec.Rout: spec.R params.rda flows.rda
	$(pipeR)

%.timevar_spec.Rout: timevar_spec.R spec.rds keydates.rda ../data/rp_%.csv params.rda
	$(pipeR)

seroprevdata.Rout: seroprevdata.R keydates.rda ../data/serop_avgcase_data.csv    
	$(pipeR)

%.calibrate.Rout: calibrate.R %.timevar_spec.rds seroprevdata.rds params.rda keydates.rda timevar_spec.R
	$(pipeR)

%.calibrate_plot.Rout: calibrate_plot.R %.calibrate.rds seroprevdata.rds keydates.rda params.rda
	$(pipeR)

%.extract_beta.Rout: extract_beta.R  %.calibrate.rds seroprevdata.rds keydates.rda params.R
        $(pipeR)

%.counterfact_sim.Rout: counterfact_sim.R %.timevar_spec.rds ../data/rp_%.csv seroprevdata.rds keydates.rda params.rda timevar_spec.R    
	$(pipeRcall)

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone --depth 1 $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
