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

# eligfrac2.timevar_spec.Rout: timevar_spec.R ../data/rp_eligfrac2.csv
%.timevar_spec.Rout: timevar_spec.R spec.rds keydates.rda ../data/rp_%.csv params.rda
	$(pipeR)

seroprevdata.Rout: seroprevdata.R keydates.rda ../data/serop_avgcase_data.csv    
	$(pipeR)

# eligfrac2.calibrate.Rout: calibrate.R
%.calibrate.Rout: calibrate.R %.timevar_spec.rds seroprevdata.rds params.rda keydates.rda timevar_spec.R
	$(pipeR)

# eligfrac2.calibrate_plot.Rout: calibrate_plot.R
%.calibrate_plot.Rout: calibrate_plot.R %.calibrate.rds seroprevdata.rds keydates.rda params.rda
	$(pipeR)

%.extract_beta.Rout: extract_beta.R  %.calibrate.rds seroprevdata.rds keydates.rda params.R
        $(pipeR)

%.counterfact_sim.Rout: counterfact_sim.R %.calibrate.rds  %.timevar_spec.rds ../data/rp_%.csv seroprevdata.rds keydates.rda params.rda timevar_spec.R  
	$(pipeR)

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
