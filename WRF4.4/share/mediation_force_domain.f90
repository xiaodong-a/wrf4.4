


SUBROUTINE med_force_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   USE module_dm, ONLY : intercomm_active, &
                         mpi_comm_to_kid, mpi_comm_to_mom, which_kid

   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags





   INTERFACE







      SUBROUTINE interp_domain_em_part1 ( grid, intermediate_grid, ngrid, config_flags   &







,wetscav_frcing,emis_ant,eghg_bio,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par, &
conv_ct,chem_ct,vmix_ct,advh_ct,advz_ct,dvel,aero_srf_area,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs, &
chem_btxe,chem_btys,chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs, &
dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs, &
scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe, &
dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,aerocu,ozmixm,aerosolc_1, &
aerosolc_2,fdda3d,fdda2d,advh_t,advz_t,pert3d,nba_mij,nba_rij,sbmradar,irr_diag_mozcart,irr_diag_t1_mozcart, &
irr_diag_mozart_mosaic_4bin,irr_diag_mozart_mosaic_4bin_aq &


                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          
         TYPE(domain), POINTER :: intermediate_grid
         TYPE(domain), POINTER :: ngrid
         TYPE (grid_config_rec_type)            :: config_flags






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_wetscav_frcing)           :: wetscav_frcing
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit,grid%sm33:grid%em33,num_emis_ant)           :: emis_ant
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_eghg_bio)           :: eghg_bio
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_dust)           :: emis_dust
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas)           :: emis_seas
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas2)           :: emis_seas2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_emis_vol)           :: emis_vol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ebu)           :: ebu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfire,grid%sm33:grid%em33,num_ebu_in)           :: ebu_in
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit_aircraft,grid%sm33:grid%em33,num_emis_aircraft)           :: emis_aircraft
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ext_coef)           :: ext_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_bscat_coef)           :: bscat_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_asym_par)           :: asym_par
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_conv_ct)           :: conv_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem_ct)           :: chem_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_vmix_ct)           :: vmix_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_ct)           :: advh_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_ct)           :: advz_ct
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kdvel,grid%sm33:grid%em33,num_dvel)           :: dvel
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aero_srf_area)           :: aero_srf_area
real      ,DIMENSION(grid%sm31:grid%em31,1:8,grid%sm33:grid%em33,num_vprm_in)           :: vprm_in
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_wet_in)           :: wet_in
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerocu)           :: aerocu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_stoch_levels,grid%sm33:grid%em33,num_pert3d)           :: pert3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_sbmradar)           :: sbmradar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozcart)           :: irr_diag_mozcart
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_t1_mozcart)           :: irr_diag_t1_mozcart
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozart_mosaic_4bin)           :: irr_diag_mozart_mosaic_4bin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozart_mosaic_4bin_aq)           :: irr_diag_mozart_mosaic_4bin_aq

      END SUBROUTINE interp_domain_em_part1

      SUBROUTINE force_domain_em_part2 ( grid, nested_grid, parent_grid, config_flags   &







,wetscav_frcing,emis_ant,eghg_bio,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par, &
conv_ct,chem_ct,vmix_ct,advh_ct,advz_ct,dvel,aero_srf_area,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs, &
chem_btxe,chem_btys,chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs, &
dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs, &
scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe, &
dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,aerocu,ozmixm,aerosolc_1, &
aerosolc_2,fdda3d,fdda2d,advh_t,advz_t,pert3d,nba_mij,nba_rij,sbmradar,irr_diag_mozcart,irr_diag_t1_mozcart, &
irr_diag_mozart_mosaic_4bin,irr_diag_mozart_mosaic_4bin_aq &


                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          
         TYPE(domain), POINTER :: nested_grid
	 TYPE(domain), POINTER :: parent_grid   
         TYPE (grid_config_rec_type)            :: config_flags






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_wetscav_frcing)           :: wetscav_frcing
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit,grid%sm33:grid%em33,num_emis_ant)           :: emis_ant
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_eghg_bio)           :: eghg_bio
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_dust)           :: emis_dust
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas)           :: emis_seas
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas2)           :: emis_seas2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_emis_vol)           :: emis_vol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ebu)           :: ebu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfire,grid%sm33:grid%em33,num_ebu_in)           :: ebu_in
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit_aircraft,grid%sm33:grid%em33,num_emis_aircraft)           :: emis_aircraft
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ext_coef)           :: ext_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_bscat_coef)           :: bscat_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_asym_par)           :: asym_par
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_conv_ct)           :: conv_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem_ct)           :: chem_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_vmix_ct)           :: vmix_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_ct)           :: advh_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_ct)           :: advz_ct
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kdvel,grid%sm33:grid%em33,num_dvel)           :: dvel
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aero_srf_area)           :: aero_srf_area
real      ,DIMENSION(grid%sm31:grid%em31,1:8,grid%sm33:grid%em33,num_vprm_in)           :: vprm_in
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_wet_in)           :: wet_in
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerocu)           :: aerocu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_stoch_levels,grid%sm33:grid%em33,num_pert3d)           :: pert3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_sbmradar)           :: sbmradar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozcart)           :: irr_diag_mozcart
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_t1_mozcart)           :: irr_diag_t1_mozcart
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozart_mosaic_4bin)           :: irr_diag_mozart_mosaic_4bin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozart_mosaic_4bin_aq)           :: irr_diag_mozart_mosaic_4bin_aq

      END SUBROUTINE force_domain_em_part2




      SUBROUTINE couple_or_uncouple_em ( grid, config_flags , couple  &







,wetscav_frcing,emis_ant,eghg_bio,emis_dust,emis_seas,emis_seas2,emis_vol,ebu,ebu_in,emis_aircraft,ext_coef,bscat_coef,asym_par, &
conv_ct,chem_ct,vmix_ct,advh_ct,advz_ct,dvel,aero_srf_area,vprm_in,wet_in,chem,chem_bxs,chem_bxe,chem_bys,chem_bye,chem_btxs, &
chem_btxe,chem_btys,chem_btye,tracer,tracer_bxs,tracer_bxe,tracer_bys,tracer_bye,tracer_btxs,tracer_btxe,tracer_btys, &
tracer_btye,moist,moist_bxs,moist_bxe,moist_bys,moist_bye,moist_btxs,moist_btxe,moist_btys,moist_btye,dfi_moist,dfi_moist_bxs, &
dfi_moist_bxe,dfi_moist_bys,dfi_moist_bye,dfi_moist_btxs,dfi_moist_btxe,dfi_moist_btys,dfi_moist_btye,scalar,scalar_bxs, &
scalar_bxe,scalar_bys,scalar_bye,scalar_btxs,scalar_btxe,scalar_btys,scalar_btye,dfi_scalar,dfi_scalar_bxs,dfi_scalar_bxe, &
dfi_scalar_bys,dfi_scalar_bye,dfi_scalar_btxs,dfi_scalar_btxe,dfi_scalar_btys,dfi_scalar_btye,aerod,aerocu,ozmixm,aerosolc_1, &
aerosolc_2,fdda3d,fdda2d,advh_t,advz_t,pert3d,nba_mij,nba_rij,sbmradar,irr_diag_mozcart,irr_diag_t1_mozcart, &
irr_diag_mozart_mosaic_4bin,irr_diag_mozart_mosaic_4bin_aq &


                 )
         USE module_domain
         USE module_configure
         TYPE(domain), INTENT(INOUT)            :: grid
         TYPE (grid_config_rec_type)            :: config_flags
         LOGICAL, INTENT(   IN) :: couple






real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_wetscav_frcing)           :: wetscav_frcing
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit,grid%sm33:grid%em33,num_emis_ant)           :: emis_ant
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_eghg_bio)           :: eghg_bio
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_dust)           :: emis_dust
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas)           :: emis_seas
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfuture,grid%sm33:grid%em33,num_emis_seas2)           :: emis_seas2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_emis_vol)           :: emis_vol
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ebu)           :: ebu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kfire,grid%sm33:grid%em33,num_ebu_in)           :: ebu_in
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kemit_aircraft,grid%sm33:grid%em33,num_emis_aircraft)           :: emis_aircraft
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_ext_coef)           :: ext_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_bscat_coef)           :: bscat_coef
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_asym_par)           :: asym_par
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_conv_ct)           :: conv_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem_ct)           :: chem_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_vmix_ct)           :: vmix_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_ct)           :: advh_ct
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_ct)           :: advz_ct
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%kdvel,grid%sm33:grid%em33,num_dvel)           :: dvel
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aero_srf_area)           :: aero_srf_area
real      ,DIMENSION(grid%sm31:grid%em31,1:8,grid%sm33:grid%em33,num_vprm_in)           :: vprm_in
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_wet_in)           :: wet_in
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_chem)           :: chem
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_chem)           :: chem_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_tracer)           :: tracer
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_tracer)           :: tracer_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_moist)           :: moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_moist)           :: moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_moist)           :: dfi_moist
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_moist)           :: dfi_moist_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_scalar)           :: scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_scalar)           :: scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_dfi_scalar)           :: dfi_scalar
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_bye
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxs
real      ,DIMENSION(grid%sm33:grid%em33,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btxe
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btys
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%spec_bdy_width,num_dfi_scalar)           :: dfi_scalar_btye
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerod)           :: aerod
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_aerocu)           :: aerocu
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%levsiz,grid%sm33:grid%em33,num_ozmixm)           :: ozmixm
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_1
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%paerlev,grid%sm33:grid%em33,num_aerosolc)           :: aerosolc_2
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_fdda3d)           :: fdda3d
real      ,DIMENSION(grid%sm31:grid%em31,1:1,grid%sm33:grid%em33,num_fdda2d)           :: fdda2d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advh_t)           :: advh_t
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_advz_t)           :: advz_t
real      ,DIMENSION(grid%sm31:grid%em31,1:grid%num_stoch_levels,grid%sm33:grid%em33,num_pert3d)           :: pert3d
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_mij)           :: nba_mij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_nba_rij)           :: nba_rij
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_sbmradar)           :: sbmradar
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozcart)           :: irr_diag_mozcart
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_t1_mozcart)           :: irr_diag_t1_mozcart
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozart_mosaic_4bin)           :: irr_diag_mozart_mosaic_4bin
real      ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,num_irr_diag_mozart_mosaic_4bin_aq)           :: irr_diag_mozart_mosaic_4bin_aq

      END SUBROUTINE couple_or_uncouple_em
   END INTERFACE










   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )

   grid => nested_grid%intermediate_grid

   
   grid => parent_grid
   IF ( grid%active_this_task ) THEN
     CALL push_communicators_for_domain(grid%id)
   
     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
     CALL couple_or_uncouple_em ( grid , config_flags ,  .true. &







,grid%wetscav_frcing,grid%emis_ant,grid%eghg_bio,grid%emis_dust,grid%emis_seas,grid%emis_seas2,grid%emis_vol,grid%ebu, &
grid%ebu_in,grid%emis_aircraft,grid%ext_coef,grid%bscat_coef,grid%asym_par,grid%conv_ct,grid%chem_ct,grid%vmix_ct,grid%advh_ct, &
grid%advz_ct,grid%dvel,grid%aero_srf_area,grid%vprm_in,grid%wet_in,grid%chem,grid%chem_bxs,grid%chem_bxe,grid%chem_bys, &
grid%chem_bye,grid%chem_btxs,grid%chem_btxe,grid%chem_btys,grid%chem_btye,grid%tracer,grid%tracer_bxs,grid%tracer_bxe, &
grid%tracer_bys,grid%tracer_bye,grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye,grid%moist,grid%moist_bxs, &
grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist, &
grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe, &
grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye, &
grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe, &
grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye, &
grid%aerod,grid%aerocu,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t,grid%pert3d, &
grid%nba_mij,grid%nba_rij,grid%sbmradar,grid%irr_diag_mozcart,grid%irr_diag_t1_mozcart,grid%irr_diag_mozart_mosaic_4bin, &
grid%irr_diag_mozart_mosaic_4bin_aq &


                                )
     CALL pop_communicators_for_domain
   ENDIF
   
   grid => nested_grid
   IF ( grid%active_this_task ) THEN
     CALL push_communicators_for_domain(grid%id)
     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
     CALL couple_or_uncouple_em ( grid , config_flags ,  .true. &







,grid%wetscav_frcing,grid%emis_ant,grid%eghg_bio,grid%emis_dust,grid%emis_seas,grid%emis_seas2,grid%emis_vol,grid%ebu, &
grid%ebu_in,grid%emis_aircraft,grid%ext_coef,grid%bscat_coef,grid%asym_par,grid%conv_ct,grid%chem_ct,grid%vmix_ct,grid%advh_ct, &
grid%advz_ct,grid%dvel,grid%aero_srf_area,grid%vprm_in,grid%wet_in,grid%chem,grid%chem_bxs,grid%chem_bxe,grid%chem_bys, &
grid%chem_bye,grid%chem_btxs,grid%chem_btxe,grid%chem_btys,grid%chem_btye,grid%tracer,grid%tracer_bxs,grid%tracer_bxe, &
grid%tracer_bys,grid%tracer_bye,grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye,grid%moist,grid%moist_bxs, &
grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist, &
grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe, &
grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye, &
grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe, &
grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye, &
grid%aerod,grid%aerocu,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t,grid%pert3d, &
grid%nba_mij,grid%nba_rij,grid%sbmradar,grid%irr_diag_mozcart,grid%irr_diag_t1_mozcart,grid%irr_diag_mozart_mosaic_4bin, &
grid%irr_diag_mozart_mosaic_4bin_aq &


                                   )
     CALL pop_communicators_for_domain
   ENDIF
   
   
   
   grid => parent_grid
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   
   

   
   
   

   IF ( parent_grid%active_this_task .AND. nested_grid%active_this_task ) THEN
     nested_grid%intermediate_grid%dt = grid%dt
   ENDIF
   IF ( parent_grid%active_this_task ) THEN
     CALL BYTE_BCAST( parent_grid%dt,4,mpi_comm_to_kid( which_kid( nested_grid%id ) , parent_grid%id ))
   ELSE IF ( nested_grid%active_this_task ) THEN
     CALL BYTE_BCAST( nested_grid%dt,4,mpi_comm_to_mom( nested_grid%id ) )
   ENDIF

   IF ( parent_grid%active_this_task .OR. nested_grid%active_this_task ) THEN
     CALL wrf_dm_nestexchange_init

     CALL interp_domain_em_part1 ( grid , nested_grid%intermediate_grid, nested_grid, config_flags  &







,grid%wetscav_frcing,grid%emis_ant,grid%eghg_bio,grid%emis_dust,grid%emis_seas,grid%emis_seas2,grid%emis_vol,grid%ebu, &
grid%ebu_in,grid%emis_aircraft,grid%ext_coef,grid%bscat_coef,grid%asym_par,grid%conv_ct,grid%chem_ct,grid%vmix_ct,grid%advh_ct, &
grid%advz_ct,grid%dvel,grid%aero_srf_area,grid%vprm_in,grid%wet_in,grid%chem,grid%chem_bxs,grid%chem_bxe,grid%chem_bys, &
grid%chem_bye,grid%chem_btxs,grid%chem_btxe,grid%chem_btys,grid%chem_btye,grid%tracer,grid%tracer_bxs,grid%tracer_bxe, &
grid%tracer_bys,grid%tracer_bye,grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye,grid%moist,grid%moist_bxs, &
grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist, &
grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe, &
grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye, &
grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe, &
grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye, &
grid%aerod,grid%aerocu,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t,grid%pert3d, &
grid%nba_mij,grid%nba_rij,grid%sbmradar,grid%irr_diag_mozcart,grid%irr_diag_t1_mozcart,grid%irr_diag_mozart_mosaic_4bin, &
grid%irr_diag_mozart_mosaic_4bin_aq &


                                    )
   ENDIF

  IF ( nested_grid%active_this_task ) THEN
   grid => nested_grid%intermediate_grid
      
      
      
   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
   CALL force_domain_em_part2 ( grid, nested_grid, parent_grid, config_flags   &







,grid%wetscav_frcing,grid%emis_ant,grid%eghg_bio,grid%emis_dust,grid%emis_seas,grid%emis_seas2,grid%emis_vol,grid%ebu, &
grid%ebu_in,grid%emis_aircraft,grid%ext_coef,grid%bscat_coef,grid%asym_par,grid%conv_ct,grid%chem_ct,grid%vmix_ct,grid%advh_ct, &
grid%advz_ct,grid%dvel,grid%aero_srf_area,grid%vprm_in,grid%wet_in,grid%chem,grid%chem_bxs,grid%chem_bxe,grid%chem_bys, &
grid%chem_bye,grid%chem_btxs,grid%chem_btxe,grid%chem_btys,grid%chem_btye,grid%tracer,grid%tracer_bxs,grid%tracer_bxe, &
grid%tracer_bys,grid%tracer_bye,grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye,grid%moist,grid%moist_bxs, &
grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist, &
grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe, &
grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye, &
grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe, &
grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye, &
grid%aerod,grid%aerocu,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t,grid%pert3d, &
grid%nba_mij,grid%nba_rij,grid%sbmradar,grid%irr_diag_mozcart,grid%irr_diag_t1_mozcart,grid%irr_diag_mozart_mosaic_4bin, &
grid%irr_diag_mozart_mosaic_4bin_aq &


                                   )
  ENDIF
   
   grid => nested_grid
   IF ( grid%active_this_task ) THEN
     CALL push_communicators_for_domain(grid%id)
     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
     CALL couple_or_uncouple_em ( grid , config_flags ,  .false.  &







,grid%wetscav_frcing,grid%emis_ant,grid%eghg_bio,grid%emis_dust,grid%emis_seas,grid%emis_seas2,grid%emis_vol,grid%ebu, &
grid%ebu_in,grid%emis_aircraft,grid%ext_coef,grid%bscat_coef,grid%asym_par,grid%conv_ct,grid%chem_ct,grid%vmix_ct,grid%advh_ct, &
grid%advz_ct,grid%dvel,grid%aero_srf_area,grid%vprm_in,grid%wet_in,grid%chem,grid%chem_bxs,grid%chem_bxe,grid%chem_bys, &
grid%chem_bye,grid%chem_btxs,grid%chem_btxe,grid%chem_btys,grid%chem_btye,grid%tracer,grid%tracer_bxs,grid%tracer_bxe, &
grid%tracer_bys,grid%tracer_bye,grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye,grid%moist,grid%moist_bxs, &
grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist, &
grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe, &
grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye, &
grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe, &
grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye, &
grid%aerod,grid%aerocu,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t,grid%pert3d, &
grid%nba_mij,grid%nba_rij,grid%sbmradar,grid%irr_diag_mozcart,grid%irr_diag_t1_mozcart,grid%irr_diag_mozart_mosaic_4bin, &
grid%irr_diag_mozart_mosaic_4bin_aq &


                                   )
     CALL pop_communicators_for_domain
   ENDIF
   
   grid => parent_grid
   IF ( grid%active_this_task ) THEN
     CALL push_communicators_for_domain(grid%id)
     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
     CALL couple_or_uncouple_em ( grid , config_flags ,  .false.  &







,grid%wetscav_frcing,grid%emis_ant,grid%eghg_bio,grid%emis_dust,grid%emis_seas,grid%emis_seas2,grid%emis_vol,grid%ebu, &
grid%ebu_in,grid%emis_aircraft,grid%ext_coef,grid%bscat_coef,grid%asym_par,grid%conv_ct,grid%chem_ct,grid%vmix_ct,grid%advh_ct, &
grid%advz_ct,grid%dvel,grid%aero_srf_area,grid%vprm_in,grid%wet_in,grid%chem,grid%chem_bxs,grid%chem_bxe,grid%chem_bys, &
grid%chem_bye,grid%chem_btxs,grid%chem_btxe,grid%chem_btys,grid%chem_btye,grid%tracer,grid%tracer_bxs,grid%tracer_bxe, &
grid%tracer_bys,grid%tracer_bye,grid%tracer_btxs,grid%tracer_btxe,grid%tracer_btys,grid%tracer_btye,grid%moist,grid%moist_bxs, &
grid%moist_bxe,grid%moist_bys,grid%moist_bye,grid%moist_btxs,grid%moist_btxe,grid%moist_btys,grid%moist_btye,grid%dfi_moist, &
grid%dfi_moist_bxs,grid%dfi_moist_bxe,grid%dfi_moist_bys,grid%dfi_moist_bye,grid%dfi_moist_btxs,grid%dfi_moist_btxe, &
grid%dfi_moist_btys,grid%dfi_moist_btye,grid%scalar,grid%scalar_bxs,grid%scalar_bxe,grid%scalar_bys,grid%scalar_bye, &
grid%scalar_btxs,grid%scalar_btxe,grid%scalar_btys,grid%scalar_btye,grid%dfi_scalar,grid%dfi_scalar_bxs,grid%dfi_scalar_bxe, &
grid%dfi_scalar_bys,grid%dfi_scalar_bye,grid%dfi_scalar_btxs,grid%dfi_scalar_btxe,grid%dfi_scalar_btys,grid%dfi_scalar_btye, &
grid%aerod,grid%aerocu,grid%ozmixm,grid%aerosolc_1,grid%aerosolc_2,grid%fdda3d,grid%fdda2d,grid%advh_t,grid%advz_t,grid%pert3d, &
grid%nba_mij,grid%nba_rij,grid%sbmradar,grid%irr_diag_mozcart,grid%irr_diag_t1_mozcart,grid%irr_diag_mozart_mosaic_4bin, &
grid%irr_diag_mozart_mosaic_4bin_aq &


                                )
     CALL pop_communicators_for_domain
   ENDIF
   IF ( nested_grid%first_force ) THEN
      nested_grid%first_force = .FALSE.
   ENDIF
   nested_grid%dtbc = 0.

   grid => nested_grid%intermediate_grid



   RETURN
END SUBROUTINE med_force_domain


