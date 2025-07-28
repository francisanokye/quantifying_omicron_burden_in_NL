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

flows.Rout: flows.R params.rda
	$(pipeR)

vacdat.Rout: vacdat.R ../data/vaccination-coverage-map.csv params.rda 
	$(pipeRcall)

## This is using serop only
timevar_spec.Rout: timevar_spec.R flows.rda vacdat.rds params.rda
	$(pipeR)

seroprevdata.Rout: seroprevdata.R params.rda ../data/raw_citf_data.csv    
	$(pipeR)

fitsero.Rout: fitsero.R ../data/raw_citf_data.csv params.rda
	$(pipeR)

calibrate.Rout: calibrate.R timevar_spec.rds seroprevdata.rds fitsero.rds params.rda
	$(pipeRcall)

bettas.Rout: bettas.R seroprevdata.rds fitsero.rds params.rda calibrate.rds
	$(pipeR)

reprod_numb.Rout: reprod_numb.R fitsero.rds params.rda calibrate.rds
	$(pipeR)

vaccination.Rout: vaccination.R calibrate.rds params.rda
	$(pipeR)

plot_model_fit.Rout: plot_model_fit.R seroprevdata.rds fitsero.rds params.rda calibrate.rds
	$(pipeR)

prevalence_plot.Rout: prevalence_plot.R params.rda calibrate.rds
	$(pipeR)

beta_plot.Rout: beta_plot.R calibrate.rds params.rda
	$(pipeRcall)

true_vs_reported_plot.Rout: true_vs_reported_plot.R calibrate.rds fitsero.rds ../data/serop_avgcase_data.csv params.rda 
	$(pipeR)

eff_Rt.Rout: eff_Rt.R calibrate.rds params.rda
	$(pipeR)

timeline.Rout: timeline.R 
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
