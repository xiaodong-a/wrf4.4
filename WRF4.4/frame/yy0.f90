!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit. Your changes to this file will be lost.
!
SUBROUTINE nl_get_gsfcrad_gocart_coupling ( id_id , gsfcrad_gocart_coupling )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gsfcrad_gocart_coupling
  INTEGER id_id
  gsfcrad_gocart_coupling = model_config_rec%gsfcrad_gocart_coupling
  RETURN
END SUBROUTINE nl_get_gsfcrad_gocart_coupling
SUBROUTINE nl_get_gsfcgce_gocart_coupling ( id_id , gsfcgce_gocart_coupling )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gsfcgce_gocart_coupling
  INTEGER id_id
  gsfcgce_gocart_coupling = model_config_rec%gsfcgce_gocart_coupling
  RETURN
END SUBROUTINE nl_get_gsfcgce_gocart_coupling
SUBROUTINE nl_get_auxinput12_inname ( id_id , auxinput12_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: auxinput12_inname
  INTEGER id_id
  auxinput12_inname = trim(model_config_rec%auxinput12_inname)
  RETURN
END SUBROUTINE nl_get_auxinput12_inname
SUBROUTINE nl_get_io_form_auxinput12 ( id_id , io_form_auxinput12 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: io_form_auxinput12
  INTEGER id_id
  io_form_auxinput12 = model_config_rec%io_form_auxinput12
  RETURN
END SUBROUTINE nl_get_io_form_auxinput12
SUBROUTINE nl_get_emi_inname ( id_id , emi_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: emi_inname
  INTEGER id_id
  emi_inname = trim(model_config_rec%emi_inname)
  RETURN
END SUBROUTINE nl_get_emi_inname
SUBROUTINE nl_get_fireemi_inname ( id_id , fireemi_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: fireemi_inname
  INTEGER id_id
  fireemi_inname = trim(model_config_rec%fireemi_inname)
  RETURN
END SUBROUTINE nl_get_fireemi_inname
SUBROUTINE nl_get_input_chem_inname ( id_id , input_chem_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: input_chem_inname
  INTEGER id_id
  input_chem_inname = trim(model_config_rec%input_chem_inname)
  RETURN
END SUBROUTINE nl_get_input_chem_inname
SUBROUTINE nl_get_emi_outname ( id_id , emi_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: emi_outname
  INTEGER id_id
  emi_outname = trim(model_config_rec%emi_outname)
  RETURN
END SUBROUTINE nl_get_emi_outname
SUBROUTINE nl_get_fireemi_outname ( id_id , fireemi_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: fireemi_outname
  INTEGER id_id
  fireemi_outname = trim(model_config_rec%fireemi_outname)
  RETURN
END SUBROUTINE nl_get_fireemi_outname
SUBROUTINE nl_get_input_chem_outname ( id_id , input_chem_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: input_chem_outname
  INTEGER id_id
  input_chem_outname = trim(model_config_rec%input_chem_outname)
  RETURN
END SUBROUTINE nl_get_input_chem_outname
SUBROUTINE nl_get_io_style_emissions ( id_id , io_style_emissions )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: io_style_emissions
  INTEGER id_id
  io_style_emissions = model_config_rec%io_style_emissions
  RETURN
END SUBROUTINE nl_get_io_style_emissions
SUBROUTINE nl_get_bioemdt ( id_id , bioemdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: bioemdt
  INTEGER id_id
  bioemdt = model_config_rec%bioemdt(id_id)
  RETURN
END SUBROUTINE nl_get_bioemdt
SUBROUTINE nl_get_photdt ( id_id , photdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: photdt
  INTEGER id_id
  photdt = model_config_rec%photdt(id_id)
  RETURN
END SUBROUTINE nl_get_photdt
SUBROUTINE nl_get_chemdt ( id_id , chemdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: chemdt
  INTEGER id_id
  chemdt = model_config_rec%chemdt(id_id)
  RETURN
END SUBROUTINE nl_get_chemdt
SUBROUTINE nl_get_ne_area ( id_id , ne_area )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ne_area
  INTEGER id_id
  ne_area = model_config_rec%ne_area
  RETURN
END SUBROUTINE nl_get_ne_area
SUBROUTINE nl_get_kemit ( id_id , kemit )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kemit
  INTEGER id_id
  kemit = model_config_rec%kemit
  RETURN
END SUBROUTINE nl_get_kemit
SUBROUTINE nl_get_nmegan ( id_id , nmegan )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nmegan
  INTEGER id_id
  nmegan = model_config_rec%nmegan
  RETURN
END SUBROUTINE nl_get_nmegan
SUBROUTINE nl_get_kfuture ( id_id , kfuture )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kfuture
  INTEGER id_id
  kfuture = model_config_rec%kfuture
  RETURN
END SUBROUTINE nl_get_kfuture
SUBROUTINE nl_get_kfire ( id_id , kfire )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kfire
  INTEGER id_id
  kfire = model_config_rec%kfire
  RETURN
END SUBROUTINE nl_get_kfire
SUBROUTINE nl_get_kemit_aircraft ( id_id , kemit_aircraft )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kemit_aircraft
  INTEGER id_id
  kemit_aircraft = model_config_rec%kemit_aircraft
  RETURN
END SUBROUTINE nl_get_kemit_aircraft
SUBROUTINE nl_get_kdvel ( id_id , kdvel )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kdvel
  INTEGER id_id
  kdvel = model_config_rec%kdvel
  RETURN
END SUBROUTINE nl_get_kdvel
SUBROUTINE nl_get_ndepvel ( id_id , ndepvel )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ndepvel
  INTEGER id_id
  ndepvel = model_config_rec%ndepvel
  RETURN
END SUBROUTINE nl_get_ndepvel
SUBROUTINE nl_get_kdepvel ( id_id , kdepvel )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kdepvel
  INTEGER id_id
  kdepvel = model_config_rec%kdepvel
  RETURN
END SUBROUTINE nl_get_kdepvel
SUBROUTINE nl_get_biomass_emiss_opt ( id_id , biomass_emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: biomass_emiss_opt
  INTEGER id_id
  biomass_emiss_opt = model_config_rec%biomass_emiss_opt(id_id)
  RETURN
END SUBROUTINE nl_get_biomass_emiss_opt
SUBROUTINE nl_get_cam_mam_mode ( id_id , cam_mam_mode )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cam_mam_mode
  INTEGER id_id
  cam_mam_mode = model_config_rec%cam_mam_mode
  RETURN
END SUBROUTINE nl_get_cam_mam_mode
SUBROUTINE nl_get_cam_mam_nspec ( id_id , cam_mam_nspec )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cam_mam_nspec
  INTEGER id_id
  cam_mam_nspec = model_config_rec%cam_mam_nspec
  RETURN
END SUBROUTINE nl_get_cam_mam_nspec
SUBROUTINE nl_get_cam_mp_mam_cpled ( id_id , cam_mp_mam_cpled )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: cam_mp_mam_cpled
  INTEGER id_id
  cam_mp_mam_cpled = model_config_rec%cam_mp_mam_cpled
  RETURN
END SUBROUTINE nl_get_cam_mp_mam_cpled
SUBROUTINE nl_get_mozart_ph_diag ( id_id , mozart_ph_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mozart_ph_diag
  INTEGER id_id
  mozart_ph_diag = model_config_rec%mozart_ph_diag
  RETURN
END SUBROUTINE nl_get_mozart_ph_diag
SUBROUTINE nl_get_megan_specifier ( id_id , megan_specifier )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: megan_specifier
  INTEGER id_id
  megan_specifier = model_config_rec%megan_specifier(id_id)
  RETURN
END SUBROUTINE nl_get_megan_specifier
SUBROUTINE nl_get_megan_factors_file ( id_id , megan_factors_file )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: megan_factors_file
  INTEGER id_id
  megan_factors_file = trim(model_config_rec%megan_factors_file)
  RETURN
END SUBROUTINE nl_get_megan_factors_file
SUBROUTINE nl_get_megan_mapped_emisfctrs ( id_id , megan_mapped_emisfctrs )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: megan_mapped_emisfctrs
  INTEGER id_id
  megan_mapped_emisfctrs = model_config_rec%megan_mapped_emisfctrs
  RETURN
END SUBROUTINE nl_get_megan_mapped_emisfctrs
SUBROUTINE nl_get_vprm_opt ( id_id , vprm_opt )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: vprm_opt
  INTEGER id_id
  vprm_opt = model_config_rec%vprm_opt(id_id)
  RETURN
END SUBROUTINE nl_get_vprm_opt
SUBROUTINE nl_get_wpeat ( id_id , wpeat )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: wpeat
  INTEGER id_id
  wpeat = model_config_rec%wpeat
  RETURN
END SUBROUTINE nl_get_wpeat
SUBROUTINE nl_get_wflood ( id_id , wflood )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: wflood
  INTEGER id_id
  wflood = model_config_rec%wflood
  RETURN
END SUBROUTINE nl_get_wflood
SUBROUTINE nl_get_term_opt ( id_id , term_opt )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: term_opt
  INTEGER id_id
  term_opt = model_config_rec%term_opt(id_id)
  RETURN
END SUBROUTINE nl_get_term_opt
SUBROUTINE nl_get_do_pvozone ( id_id , do_pvozone )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: do_pvozone
  INTEGER id_id
  do_pvozone = model_config_rec%do_pvozone
  RETURN
END SUBROUTINE nl_get_do_pvozone
SUBROUTINE nl_get_chem_conv_tr ( id_id , chem_conv_tr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: chem_conv_tr
  INTEGER id_id
  chem_conv_tr = model_config_rec%chem_conv_tr(id_id)
  RETURN
END SUBROUTINE nl_get_chem_conv_tr
SUBROUTINE nl_get_conv_tr_wetscav ( id_id , conv_tr_wetscav )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: conv_tr_wetscav
  INTEGER id_id
  conv_tr_wetscav = model_config_rec%conv_tr_wetscav(id_id)
  RETURN
END SUBROUTINE nl_get_conv_tr_wetscav
SUBROUTINE nl_get_conv_tr_aqchem ( id_id , conv_tr_aqchem )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: conv_tr_aqchem
  INTEGER id_id
  conv_tr_aqchem = model_config_rec%conv_tr_aqchem(id_id)
  RETURN
END SUBROUTINE nl_get_conv_tr_aqchem
SUBROUTINE nl_get_chem_opt ( id_id , chem_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: chem_opt
  INTEGER id_id
  chem_opt = model_config_rec%chem_opt(id_id)
  RETURN
END SUBROUTINE nl_get_chem_opt
SUBROUTINE nl_get_gaschem_onoff ( id_id , gaschem_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gaschem_onoff
  INTEGER id_id
  gaschem_onoff = model_config_rec%gaschem_onoff(id_id)
  RETURN
END SUBROUTINE nl_get_gaschem_onoff
SUBROUTINE nl_get_aerchem_onoff ( id_id , aerchem_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aerchem_onoff
  INTEGER id_id
  aerchem_onoff = model_config_rec%aerchem_onoff(id_id)
  RETURN
END SUBROUTINE nl_get_aerchem_onoff
SUBROUTINE nl_get_wetscav_onoff ( id_id , wetscav_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: wetscav_onoff
  INTEGER id_id
  wetscav_onoff = model_config_rec%wetscav_onoff(id_id)
  RETURN
END SUBROUTINE nl_get_wetscav_onoff
SUBROUTINE nl_get_dustwd_onoff ( id_id , dustwd_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dustwd_onoff
  INTEGER id_id
  dustwd_onoff = model_config_rec%dustwd_onoff(id_id)
  RETURN
END SUBROUTINE nl_get_dustwd_onoff
SUBROUTINE nl_get_cldchem_onoff ( id_id , cldchem_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cldchem_onoff
  INTEGER id_id
  cldchem_onoff = model_config_rec%cldchem_onoff(id_id)
  RETURN
END SUBROUTINE nl_get_cldchem_onoff
SUBROUTINE nl_get_is_full_tuv ( id_id , is_full_tuv )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: is_full_tuv
  INTEGER id_id
  is_full_tuv = model_config_rec%is_full_tuv(id_id)
  RETURN
END SUBROUTINE nl_get_is_full_tuv
SUBROUTINE nl_get_lambda_cutoff ( id_id , lambda_cutoff )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: lambda_cutoff
  INTEGER id_id
  lambda_cutoff = model_config_rec%lambda_cutoff(id_id)
  RETURN
END SUBROUTINE nl_get_lambda_cutoff
SUBROUTINE nl_get_cld_od_opt ( id_id , cld_od_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cld_od_opt
  INTEGER id_id
  cld_od_opt = model_config_rec%cld_od_opt(id_id)
  RETURN
END SUBROUTINE nl_get_cld_od_opt
SUBROUTINE nl_get_pht_cldfrc_opt ( id_id , pht_cldfrc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: pht_cldfrc_opt
  INTEGER id_id
  pht_cldfrc_opt = model_config_rec%pht_cldfrc_opt(id_id)
  RETURN
END SUBROUTINE nl_get_pht_cldfrc_opt
SUBROUTINE nl_get_phot_blcld ( id_id , phot_blcld )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: phot_blcld
  INTEGER id_id
  phot_blcld = model_config_rec%phot_blcld(id_id)
  RETURN
END SUBROUTINE nl_get_phot_blcld
SUBROUTINE nl_get_vertmix_onoff ( id_id , vertmix_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: vertmix_onoff
  INTEGER id_id
  vertmix_onoff = model_config_rec%vertmix_onoff(id_id)
  RETURN
END SUBROUTINE nl_get_vertmix_onoff
SUBROUTINE nl_get_chem_in_opt ( id_id , chem_in_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: chem_in_opt
  INTEGER id_id
  chem_in_opt = model_config_rec%chem_in_opt(id_id)
  RETURN
END SUBROUTINE nl_get_chem_in_opt
SUBROUTINE nl_get_phot_opt ( id_id , phot_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: phot_opt
  INTEGER id_id
  phot_opt = model_config_rec%phot_opt(id_id)
  RETURN
END SUBROUTINE nl_get_phot_opt
SUBROUTINE nl_get_gas_drydep_opt ( id_id , gas_drydep_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gas_drydep_opt
  INTEGER id_id
  gas_drydep_opt = model_config_rec%gas_drydep_opt(id_id)
  RETURN
END SUBROUTINE nl_get_gas_drydep_opt
SUBROUTINE nl_get_aer_drydep_opt ( id_id , aer_drydep_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_drydep_opt
  INTEGER id_id
  aer_drydep_opt = model_config_rec%aer_drydep_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_drydep_opt
SUBROUTINE nl_get_diagnostic_chem ( id_id , diagnostic_chem )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: diagnostic_chem
  INTEGER id_id
  diagnostic_chem = model_config_rec%diagnostic_chem(id_id)
  RETURN
END SUBROUTINE nl_get_diagnostic_chem
SUBROUTINE nl_get_aero_diag_opt ( id_id , aero_diag_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aero_diag_opt
  INTEGER id_id
  aero_diag_opt = model_config_rec%aero_diag_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aero_diag_opt
SUBROUTINE nl_get_aero_cw_diag_opt ( id_id , aero_cw_diag_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aero_cw_diag_opt
  INTEGER id_id
  aero_cw_diag_opt = model_config_rec%aero_cw_diag_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aero_cw_diag_opt
SUBROUTINE nl_get_kfcup_diag ( id_id , kfcup_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kfcup_diag
  INTEGER id_id
  kfcup_diag = model_config_rec%kfcup_diag(id_id)
  RETURN
END SUBROUTINE nl_get_kfcup_diag
SUBROUTINE nl_get_aer_aerodynres_opt ( id_id , aer_aerodynres_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_aerodynres_opt
  INTEGER id_id
  aer_aerodynres_opt = model_config_rec%aer_aerodynres_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_aerodynres_opt
SUBROUTINE nl_get_emiss_opt ( id_id , emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: emiss_opt
  INTEGER id_id
  emiss_opt = model_config_rec%emiss_opt(id_id)
  RETURN
END SUBROUTINE nl_get_emiss_opt
SUBROUTINE nl_get_emiss_opt_vol ( id_id , emiss_opt_vol )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: emiss_opt_vol
  INTEGER id_id
  emiss_opt_vol = model_config_rec%emiss_opt_vol(id_id)
  RETURN
END SUBROUTINE nl_get_emiss_opt_vol
SUBROUTINE nl_get_dust_opt ( id_id , dust_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dust_opt
  INTEGER id_id
  dust_opt = model_config_rec%dust_opt
  RETURN
END SUBROUTINE nl_get_dust_opt
SUBROUTINE nl_get_dust_schme ( id_id , dust_schme )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dust_schme
  INTEGER id_id
  dust_schme = model_config_rec%dust_schme
  RETURN
END SUBROUTINE nl_get_dust_schme
SUBROUTINE nl_get_dmsemis_opt ( id_id , dmsemis_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dmsemis_opt
  INTEGER id_id
  dmsemis_opt = model_config_rec%dmsemis_opt
  RETURN
END SUBROUTINE nl_get_dmsemis_opt
SUBROUTINE nl_get_seas_opt ( id_id , seas_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: seas_opt
  INTEGER id_id
  seas_opt = model_config_rec%seas_opt
  RETURN
END SUBROUTINE nl_get_seas_opt
SUBROUTINE nl_get_bio_emiss_opt ( id_id , bio_emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bio_emiss_opt
  INTEGER id_id
  bio_emiss_opt = model_config_rec%bio_emiss_opt(id_id)
  RETURN
END SUBROUTINE nl_get_bio_emiss_opt
SUBROUTINE nl_get_biomass_burn_opt ( id_id , biomass_burn_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: biomass_burn_opt
  INTEGER id_id
  biomass_burn_opt = model_config_rec%biomass_burn_opt(id_id)
  RETURN
END SUBROUTINE nl_get_biomass_burn_opt
SUBROUTINE nl_get_plumerisefire_frq ( id_id , plumerisefire_frq )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: plumerisefire_frq
  INTEGER id_id
  plumerisefire_frq = model_config_rec%plumerisefire_frq(id_id)
  RETURN
END SUBROUTINE nl_get_plumerisefire_frq
SUBROUTINE nl_get_emiss_inpt_opt ( id_id , emiss_inpt_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: emiss_inpt_opt
  INTEGER id_id
  emiss_inpt_opt = model_config_rec%emiss_inpt_opt(id_id)
  RETURN
END SUBROUTINE nl_get_emiss_inpt_opt
SUBROUTINE nl_get_gas_bc_opt ( id_id , gas_bc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gas_bc_opt
  INTEGER id_id
  gas_bc_opt = model_config_rec%gas_bc_opt(id_id)
  RETURN
END SUBROUTINE nl_get_gas_bc_opt
SUBROUTINE nl_get_gas_ic_opt ( id_id , gas_ic_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gas_ic_opt
  INTEGER id_id
  gas_ic_opt = model_config_rec%gas_ic_opt(id_id)
  RETURN
END SUBROUTINE nl_get_gas_ic_opt
SUBROUTINE nl_get_aer_bc_opt ( id_id , aer_bc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_bc_opt
  INTEGER id_id
  aer_bc_opt = model_config_rec%aer_bc_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_bc_opt
SUBROUTINE nl_get_aer_ic_opt ( id_id , aer_ic_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_ic_opt
  INTEGER id_id
  aer_ic_opt = model_config_rec%aer_ic_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_ic_opt
SUBROUTINE nl_get_have_bcs_chem ( id_id , have_bcs_chem )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: have_bcs_chem
  INTEGER id_id
  have_bcs_chem = model_config_rec%have_bcs_chem(id_id)
  RETURN
END SUBROUTINE nl_get_have_bcs_chem
SUBROUTINE nl_get_have_bcs_tracer ( id_id , have_bcs_tracer )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: have_bcs_tracer
  INTEGER id_id
  have_bcs_tracer = model_config_rec%have_bcs_tracer(id_id)
  RETURN
END SUBROUTINE nl_get_have_bcs_tracer
SUBROUTINE nl_get_scale_fire_emiss ( id_id , scale_fire_emiss )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: scale_fire_emiss
  INTEGER id_id
  scale_fire_emiss = model_config_rec%scale_fire_emiss(id_id)
  RETURN
END SUBROUTINE nl_get_scale_fire_emiss
SUBROUTINE nl_get_aer_ra_feedback ( id_id , aer_ra_feedback )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_ra_feedback
  INTEGER id_id
  aer_ra_feedback = model_config_rec%aer_ra_feedback(id_id)
  RETURN
END SUBROUTINE nl_get_aer_ra_feedback
SUBROUTINE nl_get_aer_op_opt ( id_id , aer_op_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_op_opt
  INTEGER id_id
  aer_op_opt = model_config_rec%aer_op_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_op_opt
SUBROUTINE nl_get_opt_pars_out ( id_id , opt_pars_out )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_pars_out
  INTEGER id_id
  opt_pars_out = model_config_rec%opt_pars_out
  RETURN
END SUBROUTINE nl_get_opt_pars_out
SUBROUTINE nl_get_diagnostic_dep ( id_id , diagnostic_dep )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: diagnostic_dep
  INTEGER id_id
  diagnostic_dep = model_config_rec%diagnostic_dep(id_id)
  RETURN
END SUBROUTINE nl_get_diagnostic_dep
SUBROUTINE nl_get_aircraft_emiss_opt ( id_id , aircraft_emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aircraft_emiss_opt
  INTEGER id_id
  aircraft_emiss_opt = model_config_rec%aircraft_emiss_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aircraft_emiss_opt
SUBROUTINE nl_get_have_bcs_upper ( id_id , have_bcs_upper )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: have_bcs_upper
  INTEGER id_id
  have_bcs_upper = model_config_rec%have_bcs_upper(id_id)
  RETURN
END SUBROUTINE nl_get_have_bcs_upper
SUBROUTINE nl_get_fixed_ubc_press ( id_id , fixed_ubc_press )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: fixed_ubc_press
  INTEGER id_id
  fixed_ubc_press = model_config_rec%fixed_ubc_press(id_id)
  RETURN
END SUBROUTINE nl_get_fixed_ubc_press
SUBROUTINE nl_get_fixed_ubc_inname ( id_id , fixed_ubc_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: fixed_ubc_inname
  INTEGER id_id
  fixed_ubc_inname = trim(model_config_rec%fixed_ubc_inname)
  RETURN
END SUBROUTINE nl_get_fixed_ubc_inname
SUBROUTINE nl_get_trop_lev_inname ( id_id , trop_lev_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: trop_lev_inname
  INTEGER id_id
  trop_lev_inname = trim(model_config_rec%trop_lev_inname)
  RETURN
END SUBROUTINE nl_get_trop_lev_inname
SUBROUTINE nl_get_exo_coldens_inname ( id_id , exo_coldens_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: exo_coldens_inname
  INTEGER id_id
  exo_coldens_inname = model_config_rec%exo_coldens_inname(id_id)
  RETURN
END SUBROUTINE nl_get_exo_coldens_inname
SUBROUTINE nl_get_wes_seasonal_inname ( id_id , wes_seasonal_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: wes_seasonal_inname
  INTEGER id_id
  wes_seasonal_inname = model_config_rec%wes_seasonal_inname(id_id)
  RETURN
END SUBROUTINE nl_get_wes_seasonal_inname
SUBROUTINE nl_get_chemdiag ( id_id , chemdiag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: chemdiag
  INTEGER id_id
  chemdiag = model_config_rec%chemdiag(id_id)
  RETURN
END SUBROUTINE nl_get_chemdiag
SUBROUTINE nl_get_aero_srf_area_diag ( id_id , aero_srf_area_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aero_srf_area_diag
  INTEGER id_id
  aero_srf_area_diag = model_config_rec%aero_srf_area_diag(id_id)
  RETURN
END SUBROUTINE nl_get_aero_srf_area_diag
SUBROUTINE nl_get_dust_alpha ( id_id , dust_alpha )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dust_alpha
  INTEGER id_id
  dust_alpha = model_config_rec%dust_alpha
  RETURN
END SUBROUTINE nl_get_dust_alpha
SUBROUTINE nl_get_dust_gamma ( id_id , dust_gamma )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dust_gamma
  INTEGER id_id
  dust_gamma = model_config_rec%dust_gamma
  RETURN
END SUBROUTINE nl_get_dust_gamma
SUBROUTINE nl_get_dust_smtune ( id_id , dust_smtune )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dust_smtune
  INTEGER id_id
  dust_smtune = model_config_rec%dust_smtune
  RETURN
END SUBROUTINE nl_get_dust_smtune
SUBROUTINE nl_get_dust_ustune ( id_id , dust_ustune )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dust_ustune
  INTEGER id_id
  dust_ustune = model_config_rec%dust_ustune
  RETURN
END SUBROUTINE nl_get_dust_ustune
SUBROUTINE nl_get_dust_dsr ( id_id , dust_dsr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dust_dsr
  INTEGER id_id
  dust_dsr = model_config_rec%dust_dsr
  RETURN
END SUBROUTINE nl_get_dust_dsr
SUBROUTINE nl_get_dust_veg ( id_id , dust_veg )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dust_veg
  INTEGER id_id
  dust_veg = model_config_rec%dust_veg
  RETURN
END SUBROUTINE nl_get_dust_veg
SUBROUTINE nl_get_dust_soils ( id_id , dust_soils )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dust_soils
  INTEGER id_id
  dust_soils = model_config_rec%dust_soils
  RETURN
END SUBROUTINE nl_get_dust_soils
SUBROUTINE nl_get_dust_smois ( id_id , dust_smois )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dust_smois
  INTEGER id_id
  dust_smois = model_config_rec%dust_smois
  RETURN
END SUBROUTINE nl_get_dust_smois
SUBROUTINE nl_get_emiss_ash_hgt ( id_id , emiss_ash_hgt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: emiss_ash_hgt
  INTEGER id_id
  emiss_ash_hgt = model_config_rec%emiss_ash_hgt
  RETURN
END SUBROUTINE nl_get_emiss_ash_hgt
SUBROUTINE nl_get_depo_fact ( id_id , depo_fact )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: depo_fact
  INTEGER id_id
  depo_fact = model_config_rec%depo_fact(id_id)
  RETURN
END SUBROUTINE nl_get_depo_fact
SUBROUTINE nl_get_track_chem_num ( id_id , track_chem_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: track_chem_num
  INTEGER id_id
  track_chem_num = model_config_rec%track_chem_num
  RETURN
END SUBROUTINE nl_get_track_chem_num
SUBROUTINE nl_get_track_chem_name ( id_id , track_chem_name )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: track_chem_name
  INTEGER id_id
  track_chem_name = model_config_rec%track_chem_name(id_id)
  RETURN
END SUBROUTINE nl_get_track_chem_name
SUBROUTINE nl_get_track_rad_num ( id_id , track_rad_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: track_rad_num
  INTEGER id_id
  track_rad_num = model_config_rec%track_rad_num
  RETURN
END SUBROUTINE nl_get_track_rad_num
SUBROUTINE nl_get_track_tuv_num ( id_id , track_tuv_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: track_tuv_num
  INTEGER id_id
  track_tuv_num = model_config_rec%track_tuv_num
  RETURN
END SUBROUTINE nl_get_track_tuv_num
SUBROUTINE nl_get_track_tuv_lev ( id_id , track_tuv_lev )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: track_tuv_lev
  INTEGER id_id
  track_tuv_lev = model_config_rec%track_tuv_lev
  RETURN
END SUBROUTINE nl_get_track_tuv_lev
SUBROUTINE nl_get_n2o5_hetchem ( id_id , n2o5_hetchem )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: n2o5_hetchem
  INTEGER id_id
  n2o5_hetchem = model_config_rec%n2o5_hetchem
  RETURN
END SUBROUTINE nl_get_n2o5_hetchem
SUBROUTINE nl_get_mosaic_aerchem_optaa ( id_id , mosaic_aerchem_optaa )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mosaic_aerchem_optaa
  INTEGER id_id
  mosaic_aerchem_optaa = model_config_rec%mosaic_aerchem_optaa
  RETURN
END SUBROUTINE nl_get_mosaic_aerchem_optaa
SUBROUTINE nl_get_af_lambda_start ( id_id , af_lambda_start )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: af_lambda_start
  INTEGER id_id
  af_lambda_start = model_config_rec%af_lambda_start(id_id)
  RETURN
END SUBROUTINE nl_get_af_lambda_start
SUBROUTINE nl_get_af_lambda_end ( id_id , af_lambda_end )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: af_lambda_end
  INTEGER id_id
  af_lambda_end = model_config_rec%af_lambda_end(id_id)
  RETURN
END SUBROUTINE nl_get_af_lambda_end
SUBROUTINE nl_get_do_isorropia ( id_id , do_isorropia )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: do_isorropia
  INTEGER id_id
  do_isorropia = model_config_rec%do_isorropia
  RETURN
END SUBROUTINE nl_get_do_isorropia
SUBROUTINE nl_get_n_ic ( id_id , n_ic )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: n_ic
  INTEGER id_id
  n_ic = model_config_rec%n_ic(id_id)
  RETURN
END SUBROUTINE nl_get_n_ic
SUBROUTINE nl_get_n_cg ( id_id , n_cg )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: n_cg
  INTEGER id_id
  n_cg = model_config_rec%n_cg(id_id)
  RETURN
END SUBROUTINE nl_get_n_cg
SUBROUTINE nl_get_lnox_opt ( id_id , lnox_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: lnox_opt
  INTEGER id_id
  lnox_opt = model_config_rec%lnox_opt(id_id)
  RETURN
END SUBROUTINE nl_get_lnox_opt
SUBROUTINE nl_get_lnox_passive ( id_id , lnox_passive )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: lnox_passive
  INTEGER id_id
  lnox_passive = model_config_rec%lnox_passive(id_id)
  RETURN
END SUBROUTINE nl_get_lnox_passive
SUBROUTINE nl_get_ltng_temp_upper ( id_id , ltng_temp_upper )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: ltng_temp_upper
  INTEGER id_id
  ltng_temp_upper = model_config_rec%ltng_temp_upper(id_id)
  RETURN
END SUBROUTINE nl_get_ltng_temp_upper
SUBROUTINE nl_get_ltng_temp_lower ( id_id , ltng_temp_lower )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: ltng_temp_lower
  INTEGER id_id
  ltng_temp_lower = model_config_rec%ltng_temp_lower(id_id)
  RETURN
END SUBROUTINE nl_get_ltng_temp_lower
SUBROUTINE nl_get_has_o3_exo_coldens ( id_id , has_o3_exo_coldens )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: has_o3_exo_coldens
  INTEGER id_id
  has_o3_exo_coldens = model_config_rec%has_o3_exo_coldens
  RETURN
END SUBROUTINE nl_get_has_o3_exo_coldens
SUBROUTINE nl_get_du_at_grnd ( id_id , du_at_grnd )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: du_at_grnd
  INTEGER id_id
  du_at_grnd = model_config_rec%du_at_grnd
  RETURN
END SUBROUTINE nl_get_du_at_grnd
SUBROUTINE nl_get_scale_o3_to_grnd_exo_coldens ( id_id , scale_o3_to_grnd_exo_coldens )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: scale_o3_to_grnd_exo_coldens
  INTEGER id_id
  scale_o3_to_grnd_exo_coldens = model_config_rec%scale_o3_to_grnd_exo_coldens
  RETURN
END SUBROUTINE nl_get_scale_o3_to_grnd_exo_coldens
SUBROUTINE nl_get_scale_o3_to_du_at_grnd ( id_id , scale_o3_to_du_at_grnd )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: scale_o3_to_du_at_grnd
  INTEGER id_id
  scale_o3_to_du_at_grnd = model_config_rec%scale_o3_to_du_at_grnd
  RETURN
END SUBROUTINE nl_get_scale_o3_to_du_at_grnd
SUBROUTINE nl_get_irr_opt ( id_id , irr_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irr_opt
  INTEGER id_id
  irr_opt = model_config_rec%irr_opt(id_id)
  RETURN
END SUBROUTINE nl_get_irr_opt
SUBROUTINE nl_get_num_gca_levels ( id_id , num_gca_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_gca_levels
  INTEGER id_id
  num_gca_levels = model_config_rec%num_gca_levels
  RETURN
END SUBROUTINE nl_get_num_gca_levels
SUBROUTINE nl_get_gca_input_opt ( id_id , gca_input_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gca_input_opt
  INTEGER id_id
  gca_input_opt = model_config_rec%gca_input_opt
  RETURN
END SUBROUTINE nl_get_gca_input_opt
SUBROUTINE nl_get_run_days ( id_id , run_days )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: run_days
  INTEGER id_id
  run_days = model_config_rec%run_days
  RETURN
END SUBROUTINE nl_get_run_days
SUBROUTINE nl_get_run_hours ( id_id , run_hours )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: run_hours
  INTEGER id_id
  run_hours = model_config_rec%run_hours
  RETURN
END SUBROUTINE nl_get_run_hours
SUBROUTINE nl_get_run_minutes ( id_id , run_minutes )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: run_minutes
  INTEGER id_id
  run_minutes = model_config_rec%run_minutes
  RETURN
END SUBROUTINE nl_get_run_minutes
SUBROUTINE nl_get_run_seconds ( id_id , run_seconds )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: run_seconds
  INTEGER id_id
  run_seconds = model_config_rec%run_seconds
  RETURN
END SUBROUTINE nl_get_run_seconds
SUBROUTINE nl_get_start_year ( id_id , start_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: start_year
  INTEGER id_id
  start_year = model_config_rec%start_year(id_id)
  RETURN
END SUBROUTINE nl_get_start_year
SUBROUTINE nl_get_start_month ( id_id , start_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: start_month
  INTEGER id_id
  start_month = model_config_rec%start_month(id_id)
  RETURN
END SUBROUTINE nl_get_start_month
SUBROUTINE nl_get_start_day ( id_id , start_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: start_day
  INTEGER id_id
  start_day = model_config_rec%start_day(id_id)
  RETURN
END SUBROUTINE nl_get_start_day
SUBROUTINE nl_get_start_hour ( id_id , start_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: start_hour
  INTEGER id_id
  start_hour = model_config_rec%start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_start_hour
SUBROUTINE nl_get_start_minute ( id_id , start_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: start_minute
  INTEGER id_id
  start_minute = model_config_rec%start_minute(id_id)
  RETURN
END SUBROUTINE nl_get_start_minute
SUBROUTINE nl_get_start_second ( id_id , start_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: start_second
  INTEGER id_id
  start_second = model_config_rec%start_second(id_id)
  RETURN
END SUBROUTINE nl_get_start_second
SUBROUTINE nl_get_end_year ( id_id , end_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: end_year
  INTEGER id_id
  end_year = model_config_rec%end_year(id_id)
  RETURN
END SUBROUTINE nl_get_end_year
SUBROUTINE nl_get_end_month ( id_id , end_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: end_month
  INTEGER id_id
  end_month = model_config_rec%end_month(id_id)
  RETURN
END SUBROUTINE nl_get_end_month
SUBROUTINE nl_get_end_day ( id_id , end_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: end_day
  INTEGER id_id
  end_day = model_config_rec%end_day(id_id)
  RETURN
END SUBROUTINE nl_get_end_day
SUBROUTINE nl_get_end_hour ( id_id , end_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: end_hour
  INTEGER id_id
  end_hour = model_config_rec%end_hour(id_id)
  RETURN
END SUBROUTINE nl_get_end_hour
SUBROUTINE nl_get_end_minute ( id_id , end_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: end_minute
  INTEGER id_id
  end_minute = model_config_rec%end_minute(id_id)
  RETURN
END SUBROUTINE nl_get_end_minute
SUBROUTINE nl_get_end_second ( id_id , end_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: end_second
  INTEGER id_id
  end_second = model_config_rec%end_second(id_id)
  RETURN
END SUBROUTINE nl_get_end_second
SUBROUTINE nl_get_interval_seconds ( id_id , interval_seconds )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: interval_seconds
  INTEGER id_id
  interval_seconds = model_config_rec%interval_seconds
  RETURN
END SUBROUTINE nl_get_interval_seconds
SUBROUTINE nl_get_input_from_file ( id_id , input_from_file )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: input_from_file
  INTEGER id_id
  input_from_file = model_config_rec%input_from_file(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_file
SUBROUTINE nl_get_fine_input_stream ( id_id , fine_input_stream )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: fine_input_stream
  INTEGER id_id
  fine_input_stream = model_config_rec%fine_input_stream(id_id)
  RETURN
END SUBROUTINE nl_get_fine_input_stream
SUBROUTINE nl_get_input_from_hires ( id_id , input_from_hires )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: input_from_hires
  INTEGER id_id
  input_from_hires = model_config_rec%input_from_hires(id_id)
  RETURN
END SUBROUTINE nl_get_input_from_hires
SUBROUTINE nl_get_rsmas_data_path ( id_id , rsmas_data_path )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: rsmas_data_path
  INTEGER id_id
  rsmas_data_path = trim(model_config_rec%rsmas_data_path)
  RETURN
END SUBROUTINE nl_get_rsmas_data_path
SUBROUTINE nl_get_all_ic_times ( id_id , all_ic_times )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: all_ic_times
  INTEGER id_id
  all_ic_times = model_config_rec%all_ic_times
  RETURN
END SUBROUTINE nl_get_all_ic_times
SUBROUTINE nl_get_julyr ( id_id , julyr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: julyr
  INTEGER id_id
  julyr = model_config_rec%julyr(id_id)
  RETURN
END SUBROUTINE nl_get_julyr
SUBROUTINE nl_get_julday ( id_id , julday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: julday
  INTEGER id_id
  julday = model_config_rec%julday(id_id)
  RETURN
END SUBROUTINE nl_get_julday
SUBROUTINE nl_get_gmt ( id_id , gmt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: gmt
  INTEGER id_id
  gmt = model_config_rec%gmt(id_id)
  RETURN
END SUBROUTINE nl_get_gmt
SUBROUTINE nl_get_input_inname ( id_id , input_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: input_inname
  INTEGER id_id
  input_inname = trim(model_config_rec%input_inname)
  RETURN
END SUBROUTINE nl_get_input_inname
SUBROUTINE nl_get_input_outname ( id_id , input_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: input_outname
  INTEGER id_id
  input_outname = trim(model_config_rec%input_outname)
  RETURN
END SUBROUTINE nl_get_input_outname
SUBROUTINE nl_get_bdy_inname ( id_id , bdy_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: bdy_inname
  INTEGER id_id
  bdy_inname = trim(model_config_rec%bdy_inname)
  RETURN
END SUBROUTINE nl_get_bdy_inname
SUBROUTINE nl_get_bdy_outname ( id_id , bdy_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: bdy_outname
  INTEGER id_id
  bdy_outname = trim(model_config_rec%bdy_outname)
  RETURN
END SUBROUTINE nl_get_bdy_outname
SUBROUTINE nl_get_rst_inname ( id_id , rst_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: rst_inname
  INTEGER id_id
  rst_inname = trim(model_config_rec%rst_inname)
  RETURN
END SUBROUTINE nl_get_rst_inname
SUBROUTINE nl_get_rst_outname ( id_id , rst_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: rst_outname
  INTEGER id_id
  rst_outname = trim(model_config_rec%rst_outname)
  RETURN
END SUBROUTINE nl_get_rst_outname
SUBROUTINE nl_get_write_input ( id_id , write_input )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: write_input
  INTEGER id_id
  write_input = model_config_rec%write_input
  RETURN
END SUBROUTINE nl_get_write_input
SUBROUTINE nl_get_write_restart_at_0h ( id_id , write_restart_at_0h )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: write_restart_at_0h
  INTEGER id_id
  write_restart_at_0h = model_config_rec%write_restart_at_0h
  RETURN
END SUBROUTINE nl_get_write_restart_at_0h
SUBROUTINE nl_get_write_hist_at_0h_rst ( id_id , write_hist_at_0h_rst )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: write_hist_at_0h_rst
  INTEGER id_id
  write_hist_at_0h_rst = model_config_rec%write_hist_at_0h_rst
  RETURN
END SUBROUTINE nl_get_write_hist_at_0h_rst
SUBROUTINE nl_get_adjust_output_times ( id_id , adjust_output_times )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: adjust_output_times
  INTEGER id_id
  adjust_output_times = model_config_rec%adjust_output_times
  RETURN
END SUBROUTINE nl_get_adjust_output_times
SUBROUTINE nl_get_adjust_input_times ( id_id , adjust_input_times )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: adjust_input_times
  INTEGER id_id
  adjust_input_times = model_config_rec%adjust_input_times
  RETURN
END SUBROUTINE nl_get_adjust_input_times
SUBROUTINE nl_get_diag_print ( id_id , diag_print )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: diag_print
  INTEGER id_id
  diag_print = model_config_rec%diag_print
  RETURN
END SUBROUTINE nl_get_diag_print
SUBROUTINE nl_get_nocolons ( id_id , nocolons )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: nocolons
  INTEGER id_id
  nocolons = model_config_rec%nocolons
  RETURN
END SUBROUTINE nl_get_nocolons
SUBROUTINE nl_get_cycling ( id_id , cycling )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: cycling
  INTEGER id_id
  cycling = model_config_rec%cycling
  RETURN
END SUBROUTINE nl_get_cycling
SUBROUTINE nl_get_output_diagnostics ( id_id , output_diagnostics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: output_diagnostics
  INTEGER id_id
  output_diagnostics = model_config_rec%output_diagnostics
  RETURN
END SUBROUTINE nl_get_output_diagnostics
SUBROUTINE nl_get_nwp_diagnostics ( id_id , nwp_diagnostics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nwp_diagnostics
  INTEGER id_id
  nwp_diagnostics = model_config_rec%nwp_diagnostics
  RETURN
END SUBROUTINE nl_get_nwp_diagnostics
SUBROUTINE nl_get_output_ready_flag ( id_id , output_ready_flag )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: output_ready_flag
  INTEGER id_id
  output_ready_flag = model_config_rec%output_ready_flag
  RETURN
END SUBROUTINE nl_get_output_ready_flag
SUBROUTINE nl_get_force_use_old_data ( id_id , force_use_old_data )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: force_use_old_data
  INTEGER id_id
  force_use_old_data = model_config_rec%force_use_old_data
  RETURN
END SUBROUTINE nl_get_force_use_old_data
SUBROUTINE nl_get_usepio ( id_id , usepio )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: usepio
  INTEGER id_id
  usepio = model_config_rec%usepio
  RETURN
END SUBROUTINE nl_get_usepio
SUBROUTINE nl_get_pioprocs ( id_id , pioprocs )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: pioprocs
  INTEGER id_id
  pioprocs = model_config_rec%pioprocs
  RETURN
END SUBROUTINE nl_get_pioprocs
SUBROUTINE nl_get_piostart ( id_id , piostart )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: piostart
  INTEGER id_id
  piostart = model_config_rec%piostart
  RETURN
END SUBROUTINE nl_get_piostart
SUBROUTINE nl_get_piostride ( id_id , piostride )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: piostride
  INTEGER id_id
  piostride = model_config_rec%piostride
  RETURN
END SUBROUTINE nl_get_piostride
SUBROUTINE nl_get_pioshift ( id_id , pioshift )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: pioshift
  INTEGER id_id
  pioshift = model_config_rec%pioshift
  RETURN
END SUBROUTINE nl_get_pioshift
SUBROUTINE nl_get_dfi_opt ( id_id , dfi_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_opt
  INTEGER id_id
  dfi_opt = model_config_rec%dfi_opt
  RETURN
END SUBROUTINE nl_get_dfi_opt
SUBROUTINE nl_get_dfi_savehydmeteors ( id_id , dfi_savehydmeteors )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_savehydmeteors
  INTEGER id_id
  dfi_savehydmeteors = model_config_rec%dfi_savehydmeteors
  RETURN
END SUBROUTINE nl_get_dfi_savehydmeteors
SUBROUTINE nl_get_dfi_nfilter ( id_id , dfi_nfilter )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_nfilter
  INTEGER id_id
  dfi_nfilter = model_config_rec%dfi_nfilter
  RETURN
END SUBROUTINE nl_get_dfi_nfilter
SUBROUTINE nl_get_dfi_write_filtered_input ( id_id , dfi_write_filtered_input )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: dfi_write_filtered_input
  INTEGER id_id
  dfi_write_filtered_input = model_config_rec%dfi_write_filtered_input
  RETURN
END SUBROUTINE nl_get_dfi_write_filtered_input
SUBROUTINE nl_get_dfi_write_dfi_history ( id_id , dfi_write_dfi_history )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: dfi_write_dfi_history
  INTEGER id_id
  dfi_write_dfi_history = model_config_rec%dfi_write_dfi_history
  RETURN
END SUBROUTINE nl_get_dfi_write_dfi_history
SUBROUTINE nl_get_dfi_cutoff_seconds ( id_id , dfi_cutoff_seconds )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_cutoff_seconds
  INTEGER id_id
  dfi_cutoff_seconds = model_config_rec%dfi_cutoff_seconds
  RETURN
END SUBROUTINE nl_get_dfi_cutoff_seconds
SUBROUTINE nl_get_dfi_time_dim ( id_id , dfi_time_dim )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_time_dim
  INTEGER id_id
  dfi_time_dim = model_config_rec%dfi_time_dim
  RETURN
END SUBROUTINE nl_get_dfi_time_dim
SUBROUTINE nl_get_dfi_fwdstop_year ( id_id , dfi_fwdstop_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_fwdstop_year
  INTEGER id_id
  dfi_fwdstop_year = model_config_rec%dfi_fwdstop_year
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_year
SUBROUTINE nl_get_dfi_fwdstop_month ( id_id , dfi_fwdstop_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_fwdstop_month
  INTEGER id_id
  dfi_fwdstop_month = model_config_rec%dfi_fwdstop_month
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_month
SUBROUTINE nl_get_dfi_fwdstop_day ( id_id , dfi_fwdstop_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_fwdstop_day
  INTEGER id_id
  dfi_fwdstop_day = model_config_rec%dfi_fwdstop_day
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_day
SUBROUTINE nl_get_dfi_fwdstop_hour ( id_id , dfi_fwdstop_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_fwdstop_hour
  INTEGER id_id
  dfi_fwdstop_hour = model_config_rec%dfi_fwdstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_hour
SUBROUTINE nl_get_dfi_fwdstop_minute ( id_id , dfi_fwdstop_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_fwdstop_minute
  INTEGER id_id
  dfi_fwdstop_minute = model_config_rec%dfi_fwdstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_minute
SUBROUTINE nl_get_dfi_fwdstop_second ( id_id , dfi_fwdstop_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_fwdstop_second
  INTEGER id_id
  dfi_fwdstop_second = model_config_rec%dfi_fwdstop_second
  RETURN
END SUBROUTINE nl_get_dfi_fwdstop_second
SUBROUTINE nl_get_dfi_bckstop_year ( id_id , dfi_bckstop_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_bckstop_year
  INTEGER id_id
  dfi_bckstop_year = model_config_rec%dfi_bckstop_year
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_year
SUBROUTINE nl_get_dfi_bckstop_month ( id_id , dfi_bckstop_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_bckstop_month
  INTEGER id_id
  dfi_bckstop_month = model_config_rec%dfi_bckstop_month
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_month
SUBROUTINE nl_get_dfi_bckstop_day ( id_id , dfi_bckstop_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_bckstop_day
  INTEGER id_id
  dfi_bckstop_day = model_config_rec%dfi_bckstop_day
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_day
SUBROUTINE nl_get_dfi_bckstop_hour ( id_id , dfi_bckstop_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_bckstop_hour
  INTEGER id_id
  dfi_bckstop_hour = model_config_rec%dfi_bckstop_hour
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_hour
SUBROUTINE nl_get_dfi_bckstop_minute ( id_id , dfi_bckstop_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_bckstop_minute
  INTEGER id_id
  dfi_bckstop_minute = model_config_rec%dfi_bckstop_minute
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_minute
SUBROUTINE nl_get_dfi_bckstop_second ( id_id , dfi_bckstop_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dfi_bckstop_second
  INTEGER id_id
  dfi_bckstop_second = model_config_rec%dfi_bckstop_second
  RETURN
END SUBROUTINE nl_get_dfi_bckstop_second
SUBROUTINE nl_get_time_step ( id_id , time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: time_step
  INTEGER id_id
  time_step = model_config_rec%time_step
  RETURN
END SUBROUTINE nl_get_time_step
SUBROUTINE nl_get_time_step_fract_num ( id_id , time_step_fract_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: time_step_fract_num
  INTEGER id_id
  time_step_fract_num = model_config_rec%time_step_fract_num
  RETURN
END SUBROUTINE nl_get_time_step_fract_num
SUBROUTINE nl_get_time_step_fract_den ( id_id , time_step_fract_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: time_step_fract_den
  INTEGER id_id
  time_step_fract_den = model_config_rec%time_step_fract_den
  RETURN
END SUBROUTINE nl_get_time_step_fract_den
SUBROUTINE nl_get_time_step_dfi ( id_id , time_step_dfi )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: time_step_dfi
  INTEGER id_id
  time_step_dfi = model_config_rec%time_step_dfi
  RETURN
END SUBROUTINE nl_get_time_step_dfi
SUBROUTINE nl_get_reasonable_time_step_ratio ( id_id , reasonable_time_step_ratio )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: reasonable_time_step_ratio
  INTEGER id_id
  reasonable_time_step_ratio = model_config_rec%reasonable_time_step_ratio
  RETURN
END SUBROUTINE nl_get_reasonable_time_step_ratio
SUBROUTINE nl_get_min_time_step ( id_id , min_time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: min_time_step
  INTEGER id_id
  min_time_step = model_config_rec%min_time_step(id_id)
  RETURN
END SUBROUTINE nl_get_min_time_step
SUBROUTINE nl_get_min_time_step_den ( id_id , min_time_step_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: min_time_step_den
  INTEGER id_id
  min_time_step_den = model_config_rec%min_time_step_den(id_id)
  RETURN
END SUBROUTINE nl_get_min_time_step_den
SUBROUTINE nl_get_max_time_step ( id_id , max_time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: max_time_step
  INTEGER id_id
  max_time_step = model_config_rec%max_time_step(id_id)
  RETURN
END SUBROUTINE nl_get_max_time_step
SUBROUTINE nl_get_max_time_step_den ( id_id , max_time_step_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: max_time_step_den
  INTEGER id_id
  max_time_step_den = model_config_rec%max_time_step_den(id_id)
  RETURN
END SUBROUTINE nl_get_max_time_step_den
SUBROUTINE nl_get_target_cfl ( id_id , target_cfl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: target_cfl
  INTEGER id_id
  target_cfl = model_config_rec%target_cfl(id_id)
  RETURN
END SUBROUTINE nl_get_target_cfl
SUBROUTINE nl_get_target_hcfl ( id_id , target_hcfl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: target_hcfl
  INTEGER id_id
  target_hcfl = model_config_rec%target_hcfl(id_id)
  RETURN
END SUBROUTINE nl_get_target_hcfl
SUBROUTINE nl_get_max_step_increase_pct ( id_id , max_step_increase_pct )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: max_step_increase_pct
  INTEGER id_id
  max_step_increase_pct = model_config_rec%max_step_increase_pct(id_id)
  RETURN
END SUBROUTINE nl_get_max_step_increase_pct
SUBROUTINE nl_get_starting_time_step ( id_id , starting_time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: starting_time_step
  INTEGER id_id
  starting_time_step = model_config_rec%starting_time_step(id_id)
  RETURN
END SUBROUTINE nl_get_starting_time_step
SUBROUTINE nl_get_starting_time_step_den ( id_id , starting_time_step_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: starting_time_step_den
  INTEGER id_id
  starting_time_step_den = model_config_rec%starting_time_step_den(id_id)
  RETURN
END SUBROUTINE nl_get_starting_time_step_den
SUBROUTINE nl_get_step_to_output_time ( id_id , step_to_output_time )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: step_to_output_time
  INTEGER id_id
  step_to_output_time = model_config_rec%step_to_output_time
  RETURN
END SUBROUTINE nl_get_step_to_output_time
SUBROUTINE nl_get_adaptation_domain ( id_id , adaptation_domain )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: adaptation_domain
  INTEGER id_id
  adaptation_domain = model_config_rec%adaptation_domain
  RETURN
END SUBROUTINE nl_get_adaptation_domain
SUBROUTINE nl_get_use_adaptive_time_step ( id_id , use_adaptive_time_step )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: use_adaptive_time_step
  INTEGER id_id
  use_adaptive_time_step = model_config_rec%use_adaptive_time_step
  RETURN
END SUBROUTINE nl_get_use_adaptive_time_step
SUBROUTINE nl_get_use_adaptive_time_step_dfi ( id_id , use_adaptive_time_step_dfi )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: use_adaptive_time_step_dfi
  INTEGER id_id
  use_adaptive_time_step_dfi = model_config_rec%use_adaptive_time_step_dfi
  RETURN
END SUBROUTINE nl_get_use_adaptive_time_step_dfi
SUBROUTINE nl_get_max_dom ( id_id , max_dom )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: max_dom
  INTEGER id_id
  max_dom = model_config_rec%max_dom
  RETURN
END SUBROUTINE nl_get_max_dom
SUBROUTINE nl_get_lats_to_mic ( id_id , lats_to_mic )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: lats_to_mic
  INTEGER id_id
  lats_to_mic = model_config_rec%lats_to_mic
  RETURN
END SUBROUTINE nl_get_lats_to_mic
SUBROUTINE nl_get_s_we ( id_id , s_we )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: s_we
  INTEGER id_id
  s_we = model_config_rec%s_we(id_id)
  RETURN
END SUBROUTINE nl_get_s_we
SUBROUTINE nl_get_e_we ( id_id , e_we )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: e_we
  INTEGER id_id
  e_we = model_config_rec%e_we(id_id)
  RETURN
END SUBROUTINE nl_get_e_we
SUBROUTINE nl_get_s_sn ( id_id , s_sn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: s_sn
  INTEGER id_id
  s_sn = model_config_rec%s_sn(id_id)
  RETURN
END SUBROUTINE nl_get_s_sn
SUBROUTINE nl_get_e_sn ( id_id , e_sn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: e_sn
  INTEGER id_id
  e_sn = model_config_rec%e_sn(id_id)
  RETURN
END SUBROUTINE nl_get_e_sn
SUBROUTINE nl_get_s_vert ( id_id , s_vert )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: s_vert
  INTEGER id_id
  s_vert = model_config_rec%s_vert(id_id)
  RETURN
END SUBROUTINE nl_get_s_vert
SUBROUTINE nl_get_e_vert ( id_id , e_vert )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: e_vert
  INTEGER id_id
  e_vert = model_config_rec%e_vert(id_id)
  RETURN
END SUBROUTINE nl_get_e_vert
SUBROUTINE nl_get_num_metgrid_levels ( id_id , num_metgrid_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_metgrid_levels
  INTEGER id_id
  num_metgrid_levels = model_config_rec%num_metgrid_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_levels
SUBROUTINE nl_get_num_metgrid_soil_levels ( id_id , num_metgrid_soil_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_metgrid_soil_levels
  INTEGER id_id
  num_metgrid_soil_levels = model_config_rec%num_metgrid_soil_levels
  RETURN
END SUBROUTINE nl_get_num_metgrid_soil_levels
SUBROUTINE nl_get_p_top_requested ( id_id , p_top_requested )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: p_top_requested
  INTEGER id_id
  p_top_requested = model_config_rec%p_top_requested
  RETURN
END SUBROUTINE nl_get_p_top_requested
SUBROUTINE nl_get_interp_theta ( id_id , interp_theta )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: interp_theta
  INTEGER id_id
  interp_theta = model_config_rec%interp_theta
  RETURN
END SUBROUTINE nl_get_interp_theta
SUBROUTINE nl_get_interp_type ( id_id , interp_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: interp_type
  INTEGER id_id
  interp_type = model_config_rec%interp_type
  RETURN
END SUBROUTINE nl_get_interp_type
SUBROUTINE nl_get_rebalance ( id_id , rebalance )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: rebalance
  INTEGER id_id
  rebalance = model_config_rec%rebalance
  RETURN
END SUBROUTINE nl_get_rebalance
SUBROUTINE nl_get_vert_refine_method ( id_id , vert_refine_method )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: vert_refine_method
  INTEGER id_id
  vert_refine_method = model_config_rec%vert_refine_method(id_id)
  RETURN
END SUBROUTINE nl_get_vert_refine_method
SUBROUTINE nl_get_vert_refine_fact ( id_id , vert_refine_fact )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: vert_refine_fact
  INTEGER id_id
  vert_refine_fact = model_config_rec%vert_refine_fact
  RETURN
END SUBROUTINE nl_get_vert_refine_fact
SUBROUTINE nl_get_extrap_type ( id_id , extrap_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: extrap_type
  INTEGER id_id
  extrap_type = model_config_rec%extrap_type
  RETURN
END SUBROUTINE nl_get_extrap_type
SUBROUTINE nl_get_t_extrap_type ( id_id , t_extrap_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: t_extrap_type
  INTEGER id_id
  t_extrap_type = model_config_rec%t_extrap_type
  RETURN
END SUBROUTINE nl_get_t_extrap_type
SUBROUTINE nl_get_hypsometric_opt ( id_id , hypsometric_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: hypsometric_opt
  INTEGER id_id
  hypsometric_opt = model_config_rec%hypsometric_opt
  RETURN
END SUBROUTINE nl_get_hypsometric_opt
SUBROUTINE nl_get_lowest_lev_from_sfc ( id_id , lowest_lev_from_sfc )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: lowest_lev_from_sfc
  INTEGER id_id
  lowest_lev_from_sfc = model_config_rec%lowest_lev_from_sfc
  RETURN
END SUBROUTINE nl_get_lowest_lev_from_sfc
SUBROUTINE nl_get_use_levels_below_ground ( id_id , use_levels_below_ground )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: use_levels_below_ground
  INTEGER id_id
  use_levels_below_ground = model_config_rec%use_levels_below_ground
  RETURN
END SUBROUTINE nl_get_use_levels_below_ground
SUBROUTINE nl_get_use_tavg_for_tsk ( id_id , use_tavg_for_tsk )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: use_tavg_for_tsk
  INTEGER id_id
  use_tavg_for_tsk = model_config_rec%use_tavg_for_tsk
  RETURN
END SUBROUTINE nl_get_use_tavg_for_tsk
SUBROUTINE nl_get_use_surface ( id_id , use_surface )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: use_surface
  INTEGER id_id
  use_surface = model_config_rec%use_surface
  RETURN
END SUBROUTINE nl_get_use_surface
SUBROUTINE nl_get_lagrange_order ( id_id , lagrange_order )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: lagrange_order
  INTEGER id_id
  lagrange_order = model_config_rec%lagrange_order
  RETURN
END SUBROUTINE nl_get_lagrange_order
SUBROUTINE nl_get_linear_interp ( id_id , linear_interp )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: linear_interp
  INTEGER id_id
  linear_interp = model_config_rec%linear_interp
  RETURN
END SUBROUTINE nl_get_linear_interp
SUBROUTINE nl_get_force_sfc_in_vinterp ( id_id , force_sfc_in_vinterp )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: force_sfc_in_vinterp
  INTEGER id_id
  force_sfc_in_vinterp = model_config_rec%force_sfc_in_vinterp
  RETURN
END SUBROUTINE nl_get_force_sfc_in_vinterp
SUBROUTINE nl_get_zap_close_levels ( id_id , zap_close_levels )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: zap_close_levels
  INTEGER id_id
  zap_close_levels = model_config_rec%zap_close_levels
  RETURN
END SUBROUTINE nl_get_zap_close_levels
SUBROUTINE nl_get_maxw_horiz_pres_diff ( id_id , maxw_horiz_pres_diff )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: maxw_horiz_pres_diff
  INTEGER id_id
  maxw_horiz_pres_diff = model_config_rec%maxw_horiz_pres_diff
  RETURN
END SUBROUTINE nl_get_maxw_horiz_pres_diff
SUBROUTINE nl_get_trop_horiz_pres_diff ( id_id , trop_horiz_pres_diff )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: trop_horiz_pres_diff
  INTEGER id_id
  trop_horiz_pres_diff = model_config_rec%trop_horiz_pres_diff
  RETURN
END SUBROUTINE nl_get_trop_horiz_pres_diff
SUBROUTINE nl_get_maxw_above_this_level ( id_id , maxw_above_this_level )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: maxw_above_this_level
  INTEGER id_id
  maxw_above_this_level = model_config_rec%maxw_above_this_level
  RETURN
END SUBROUTINE nl_get_maxw_above_this_level
SUBROUTINE nl_get_use_maxw_level ( id_id , use_maxw_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: use_maxw_level
  INTEGER id_id
  use_maxw_level = model_config_rec%use_maxw_level
  RETURN
END SUBROUTINE nl_get_use_maxw_level
SUBROUTINE nl_get_use_trop_level ( id_id , use_trop_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: use_trop_level
  INTEGER id_id
  use_trop_level = model_config_rec%use_trop_level
  RETURN
END SUBROUTINE nl_get_use_trop_level
SUBROUTINE nl_get_sfcp_to_sfcp ( id_id , sfcp_to_sfcp )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: sfcp_to_sfcp
  INTEGER id_id
  sfcp_to_sfcp = model_config_rec%sfcp_to_sfcp
  RETURN
END SUBROUTINE nl_get_sfcp_to_sfcp
SUBROUTINE nl_get_adjust_heights ( id_id , adjust_heights )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: adjust_heights
  INTEGER id_id
  adjust_heights = model_config_rec%adjust_heights
  RETURN
END SUBROUTINE nl_get_adjust_heights
SUBROUTINE nl_get_smooth_cg_topo ( id_id , smooth_cg_topo )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: smooth_cg_topo
  INTEGER id_id
  smooth_cg_topo = model_config_rec%smooth_cg_topo
  RETURN
END SUBROUTINE nl_get_smooth_cg_topo
SUBROUTINE nl_get_nest_interp_coord ( id_id , nest_interp_coord )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nest_interp_coord
  INTEGER id_id
  nest_interp_coord = model_config_rec%nest_interp_coord
  RETURN
END SUBROUTINE nl_get_nest_interp_coord
SUBROUTINE nl_get_interp_method_type ( id_id , interp_method_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: interp_method_type
  INTEGER id_id
  interp_method_type = model_config_rec%interp_method_type
  RETURN
END SUBROUTINE nl_get_interp_method_type
SUBROUTINE nl_get_aggregate_lu ( id_id , aggregate_lu )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: aggregate_lu
  INTEGER id_id
  aggregate_lu = model_config_rec%aggregate_lu
  RETURN
END SUBROUTINE nl_get_aggregate_lu
SUBROUTINE nl_get_rh2qv_wrt_liquid ( id_id , rh2qv_wrt_liquid )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: rh2qv_wrt_liquid
  INTEGER id_id
  rh2qv_wrt_liquid = model_config_rec%rh2qv_wrt_liquid
  RETURN
END SUBROUTINE nl_get_rh2qv_wrt_liquid
SUBROUTINE nl_get_rh2qv_method ( id_id , rh2qv_method )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: rh2qv_method
  INTEGER id_id
  rh2qv_method = model_config_rec%rh2qv_method
  RETURN
END SUBROUTINE nl_get_rh2qv_method
SUBROUTINE nl_get_qv_max_p_safe ( id_id , qv_max_p_safe )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: qv_max_p_safe
  INTEGER id_id
  qv_max_p_safe = model_config_rec%qv_max_p_safe
  RETURN
END SUBROUTINE nl_get_qv_max_p_safe
SUBROUTINE nl_get_qv_max_flag ( id_id , qv_max_flag )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: qv_max_flag
  INTEGER id_id
  qv_max_flag = model_config_rec%qv_max_flag
  RETURN
END SUBROUTINE nl_get_qv_max_flag
SUBROUTINE nl_get_qv_max_value ( id_id , qv_max_value )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: qv_max_value
  INTEGER id_id
  qv_max_value = model_config_rec%qv_max_value
  RETURN
END SUBROUTINE nl_get_qv_max_value
SUBROUTINE nl_get_qv_min_p_safe ( id_id , qv_min_p_safe )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: qv_min_p_safe
  INTEGER id_id
  qv_min_p_safe = model_config_rec%qv_min_p_safe
  RETURN
END SUBROUTINE nl_get_qv_min_p_safe
SUBROUTINE nl_get_qv_min_flag ( id_id , qv_min_flag )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: qv_min_flag
  INTEGER id_id
  qv_min_flag = model_config_rec%qv_min_flag
  RETURN
END SUBROUTINE nl_get_qv_min_flag
SUBROUTINE nl_get_qv_min_value ( id_id , qv_min_value )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: qv_min_value
  INTEGER id_id
  qv_min_value = model_config_rec%qv_min_value
  RETURN
END SUBROUTINE nl_get_qv_min_value
SUBROUTINE nl_get_ideal_init_method ( id_id , ideal_init_method )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ideal_init_method
  INTEGER id_id
  ideal_init_method = model_config_rec%ideal_init_method
  RETURN
END SUBROUTINE nl_get_ideal_init_method
SUBROUTINE nl_get_dx ( id_id , dx )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dx
  INTEGER id_id
  dx = model_config_rec%dx(id_id)
  RETURN
END SUBROUTINE nl_get_dx
SUBROUTINE nl_get_dy ( id_id , dy )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dy
  INTEGER id_id
  dy = model_config_rec%dy(id_id)
  RETURN
END SUBROUTINE nl_get_dy
SUBROUTINE nl_get_grid_id ( id_id , grid_id )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: grid_id
  INTEGER id_id
  grid_id = model_config_rec%grid_id(id_id)
  RETURN
END SUBROUTINE nl_get_grid_id
SUBROUTINE nl_get_grid_allowed ( id_id , grid_allowed )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: grid_allowed
  INTEGER id_id
  grid_allowed = model_config_rec%grid_allowed(id_id)
  RETURN
END SUBROUTINE nl_get_grid_allowed
SUBROUTINE nl_get_parent_id ( id_id , parent_id )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: parent_id
  INTEGER id_id
  parent_id = model_config_rec%parent_id(id_id)
  RETURN
END SUBROUTINE nl_get_parent_id
SUBROUTINE nl_get_i_parent_start ( id_id , i_parent_start )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: i_parent_start
  INTEGER id_id
  i_parent_start = model_config_rec%i_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_i_parent_start
SUBROUTINE nl_get_j_parent_start ( id_id , j_parent_start )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: j_parent_start
  INTEGER id_id
  j_parent_start = model_config_rec%j_parent_start(id_id)
  RETURN
END SUBROUTINE nl_get_j_parent_start
SUBROUTINE nl_get_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: parent_grid_ratio
  INTEGER id_id
  parent_grid_ratio = model_config_rec%parent_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_grid_ratio
SUBROUTINE nl_get_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: parent_time_step_ratio
  INTEGER id_id
  parent_time_step_ratio = model_config_rec%parent_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_parent_time_step_ratio
SUBROUTINE nl_get_feedback ( id_id , feedback )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: feedback
  INTEGER id_id
  feedback = model_config_rec%feedback
  RETURN
END SUBROUTINE nl_get_feedback
SUBROUTINE nl_get_smooth_option ( id_id , smooth_option )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: smooth_option
  INTEGER id_id
  smooth_option = model_config_rec%smooth_option
  RETURN
END SUBROUTINE nl_get_smooth_option
SUBROUTINE nl_get_blend_width ( id_id , blend_width )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: blend_width
  INTEGER id_id
  blend_width = model_config_rec%blend_width
  RETURN
END SUBROUTINE nl_get_blend_width
SUBROUTINE nl_get_ztop ( id_id , ztop )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: ztop
  INTEGER id_id
  ztop = model_config_rec%ztop(id_id)
  RETURN
END SUBROUTINE nl_get_ztop
SUBROUTINE nl_get_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: moad_grid_ratio
  INTEGER id_id
  moad_grid_ratio = model_config_rec%moad_grid_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_grid_ratio
SUBROUTINE nl_get_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: moad_time_step_ratio
  INTEGER id_id
  moad_time_step_ratio = model_config_rec%moad_time_step_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_moad_time_step_ratio
SUBROUTINE nl_get_shw ( id_id , shw )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: shw
  INTEGER id_id
  shw = model_config_rec%shw(id_id)
  RETURN
END SUBROUTINE nl_get_shw
SUBROUTINE nl_get_tile_sz_x ( id_id , tile_sz_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: tile_sz_x
  INTEGER id_id
  tile_sz_x = model_config_rec%tile_sz_x
  RETURN
END SUBROUTINE nl_get_tile_sz_x
SUBROUTINE nl_get_tile_sz_y ( id_id , tile_sz_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: tile_sz_y
  INTEGER id_id
  tile_sz_y = model_config_rec%tile_sz_y
  RETURN
END SUBROUTINE nl_get_tile_sz_y
SUBROUTINE nl_get_numtiles ( id_id , numtiles )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: numtiles
  INTEGER id_id
  numtiles = model_config_rec%numtiles
  RETURN
END SUBROUTINE nl_get_numtiles
SUBROUTINE nl_get_numtiles_inc ( id_id , numtiles_inc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: numtiles_inc
  INTEGER id_id
  numtiles_inc = model_config_rec%numtiles_inc
  RETURN
END SUBROUTINE nl_get_numtiles_inc
SUBROUTINE nl_get_numtiles_x ( id_id , numtiles_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: numtiles_x
  INTEGER id_id
  numtiles_x = model_config_rec%numtiles_x
  RETURN
END SUBROUTINE nl_get_numtiles_x
SUBROUTINE nl_get_numtiles_y ( id_id , numtiles_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: numtiles_y
  INTEGER id_id
  numtiles_y = model_config_rec%numtiles_y
  RETURN
END SUBROUTINE nl_get_numtiles_y
SUBROUTINE nl_get_tile_strategy ( id_id , tile_strategy )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: tile_strategy
  INTEGER id_id
  tile_strategy = model_config_rec%tile_strategy
  RETURN
END SUBROUTINE nl_get_tile_strategy
SUBROUTINE nl_get_nproc_x ( id_id , nproc_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nproc_x
  INTEGER id_id
  nproc_x = model_config_rec%nproc_x
  RETURN
END SUBROUTINE nl_get_nproc_x
SUBROUTINE nl_get_nproc_y ( id_id , nproc_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nproc_y
  INTEGER id_id
  nproc_y = model_config_rec%nproc_y
  RETURN
END SUBROUTINE nl_get_nproc_y
SUBROUTINE nl_get_irand ( id_id , irand )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irand
  INTEGER id_id
  irand = model_config_rec%irand
  RETURN
END SUBROUTINE nl_get_irand
SUBROUTINE nl_get_dt ( id_id , dt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dt
  INTEGER id_id
  dt = model_config_rec%dt(id_id)
  RETURN
END SUBROUTINE nl_get_dt
SUBROUTINE nl_get_fft_used ( id_id , fft_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: fft_used
  INTEGER id_id
  fft_used = model_config_rec%fft_used
  RETURN
END SUBROUTINE nl_get_fft_used
SUBROUTINE nl_get_cu_used ( id_id , cu_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cu_used
  INTEGER id_id
  cu_used = model_config_rec%cu_used
  RETURN
END SUBROUTINE nl_get_cu_used
SUBROUTINE nl_get_shcu_used ( id_id , shcu_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: shcu_used
  INTEGER id_id
  shcu_used = model_config_rec%shcu_used
  RETURN
END SUBROUTINE nl_get_shcu_used
SUBROUTINE nl_get_cam_used ( id_id , cam_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cam_used
  INTEGER id_id
  cam_used = model_config_rec%cam_used
  RETURN
END SUBROUTINE nl_get_cam_used
SUBROUTINE nl_get_gwd_used ( id_id , gwd_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gwd_used
  INTEGER id_id
  gwd_used = model_config_rec%gwd_used
  RETURN
END SUBROUTINE nl_get_gwd_used
SUBROUTINE nl_get_gwd_diags_used ( id_id , gwd_diags_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gwd_diags_used
  INTEGER id_id
  gwd_diags_used = model_config_rec%gwd_diags_used
  RETURN
END SUBROUTINE nl_get_gwd_diags_used
SUBROUTINE nl_get_alloc_qndropsource ( id_id , alloc_qndropsource )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: alloc_qndropsource
  INTEGER id_id
  alloc_qndropsource = model_config_rec%alloc_qndropsource
  RETURN
END SUBROUTINE nl_get_alloc_qndropsource
SUBROUTINE nl_get_num_moves ( id_id , num_moves )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_moves
  INTEGER id_id
  num_moves = model_config_rec%num_moves
  RETURN
END SUBROUTINE nl_get_num_moves
SUBROUTINE nl_get_ts_buf_size ( id_id , ts_buf_size )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ts_buf_size
  INTEGER id_id
  ts_buf_size = model_config_rec%ts_buf_size
  RETURN
END SUBROUTINE nl_get_ts_buf_size
SUBROUTINE nl_get_max_ts_locs ( id_id , max_ts_locs )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: max_ts_locs
  INTEGER id_id
  max_ts_locs = model_config_rec%max_ts_locs
  RETURN
END SUBROUTINE nl_get_max_ts_locs
SUBROUTINE nl_get_tslist_ij ( id_id , tslist_ij )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: tslist_ij
  INTEGER id_id
  tslist_ij = model_config_rec%tslist_ij
  RETURN
END SUBROUTINE nl_get_tslist_ij
SUBROUTINE nl_get_tslist_unstagger_winds ( id_id , tslist_unstagger_winds )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: tslist_unstagger_winds
  INTEGER id_id
  tslist_unstagger_winds = model_config_rec%tslist_unstagger_winds
  RETURN
END SUBROUTINE nl_get_tslist_unstagger_winds
SUBROUTINE nl_get_vortex_interval ( id_id , vortex_interval )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: vortex_interval
  INTEGER id_id
  vortex_interval = model_config_rec%vortex_interval(id_id)
  RETURN
END SUBROUTINE nl_get_vortex_interval
!ENDOFREGISTRYGENERATEDINCLUDE
