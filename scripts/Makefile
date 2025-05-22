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

timevar_spec.Rout: timevar_spec.R spec.rds keydates.rda ../data/rp_base.csv params.rda
	$(pipeR)

seroprevdata.Rout: seroprevdata.R keydates.rda ../data/serop_avgcase_data.csv    
	$(pipeR)

calibrate.Rout: calibrate.R timevar_spec.rds seroprevdata.rds params.rda keydates.rda timevar_spec.R
	$(pipeR)

plot_model_fit.Rout: plot_model_fit.R ../data/serop_avgcase_data.csv keydates.rda params.rda calibrate.rds
	$(pipeR)

prevalence_plot.Rout: prevalence_plot.R ../data/serop_avgcase_data.csv keydates.rda params.rda calibrate.rds
	$(pipeR)

beta_plot.Rout: beta_plot.R ../outputs/true_infections_data.csv
	$(pipeR)

true_vs_reported_plot.Rout: true_vs_reported_plot.R ../data/serop_avgcase_data.csv ../outputs/true_infections_data.csv 
	$(pipeR)

eff_Rt.Rout: eff_Rt.R ../outputs/true_infections_data.csv
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
