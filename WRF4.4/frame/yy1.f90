!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit. Your changes to this file will be lost.
!
SUBROUTINE nl_get_max_vortex_speed ( id_id , max_vortex_speed )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: max_vortex_speed
  INTEGER id_id
  max_vortex_speed = model_config_rec%max_vortex_speed(id_id)
  RETURN
END SUBROUTINE nl_get_max_vortex_speed
SUBROUTINE nl_get_corral_dist ( id_id , corral_dist )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: corral_dist
  INTEGER id_id
  corral_dist = model_config_rec%corral_dist(id_id)
  RETURN
END SUBROUTINE nl_get_corral_dist
SUBROUTINE nl_get_track_level ( id_id , track_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: track_level
  INTEGER id_id
  track_level = model_config_rec%track_level
  RETURN
END SUBROUTINE nl_get_track_level
SUBROUTINE nl_get_time_to_move ( id_id , time_to_move )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: time_to_move
  INTEGER id_id
  time_to_move = model_config_rec%time_to_move(id_id)
  RETURN
END SUBROUTINE nl_get_time_to_move
SUBROUTINE nl_get_move_id ( id_id , move_id )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: move_id
  INTEGER id_id
  move_id = model_config_rec%move_id(id_id)
  RETURN
END SUBROUTINE nl_get_move_id
SUBROUTINE nl_get_move_interval ( id_id , move_interval )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: move_interval
  INTEGER id_id
  move_interval = model_config_rec%move_interval(id_id)
  RETURN
END SUBROUTINE nl_get_move_interval
SUBROUTINE nl_get_move_cd_x ( id_id , move_cd_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: move_cd_x
  INTEGER id_id
  move_cd_x = model_config_rec%move_cd_x(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_x
SUBROUTINE nl_get_move_cd_y ( id_id , move_cd_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: move_cd_y
  INTEGER id_id
  move_cd_y = model_config_rec%move_cd_y(id_id)
  RETURN
END SUBROUTINE nl_get_move_cd_y
SUBROUTINE nl_get_swap_x ( id_id , swap_x )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: swap_x
  INTEGER id_id
  swap_x = model_config_rec%swap_x(id_id)
  RETURN
END SUBROUTINE nl_get_swap_x
SUBROUTINE nl_get_swap_y ( id_id , swap_y )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: swap_y
  INTEGER id_id
  swap_y = model_config_rec%swap_y(id_id)
  RETURN
END SUBROUTINE nl_get_swap_y
SUBROUTINE nl_get_cycle_x ( id_id , cycle_x )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: cycle_x
  INTEGER id_id
  cycle_x = model_config_rec%cycle_x(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_x
SUBROUTINE nl_get_cycle_y ( id_id , cycle_y )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: cycle_y
  INTEGER id_id
  cycle_y = model_config_rec%cycle_y(id_id)
  RETURN
END SUBROUTINE nl_get_cycle_y
SUBROUTINE nl_get_reorder_mesh ( id_id , reorder_mesh )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: reorder_mesh
  INTEGER id_id
  reorder_mesh = model_config_rec%reorder_mesh
  RETURN
END SUBROUTINE nl_get_reorder_mesh
SUBROUTINE nl_get_perturb_input ( id_id , perturb_input )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: perturb_input
  INTEGER id_id
  perturb_input = model_config_rec%perturb_input
  RETURN
END SUBROUTINE nl_get_perturb_input
SUBROUTINE nl_get_eta_levels ( id_id , eta_levels )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: eta_levels
  INTEGER id_id
  eta_levels = model_config_rec%eta_levels(id_id)
  RETURN
END SUBROUTINE nl_get_eta_levels
SUBROUTINE nl_get_auto_levels_opt ( id_id , auto_levels_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: auto_levels_opt
  INTEGER id_id
  auto_levels_opt = model_config_rec%auto_levels_opt
  RETURN
END SUBROUTINE nl_get_auto_levels_opt
SUBROUTINE nl_get_max_dz ( id_id , max_dz )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: max_dz
  INTEGER id_id
  max_dz = model_config_rec%max_dz
  RETURN
END SUBROUTINE nl_get_max_dz
SUBROUTINE nl_get_dzbot ( id_id , dzbot )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dzbot
  INTEGER id_id
  dzbot = model_config_rec%dzbot
  RETURN
END SUBROUTINE nl_get_dzbot
SUBROUTINE nl_get_dzstretch_s ( id_id , dzstretch_s )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dzstretch_s
  INTEGER id_id
  dzstretch_s = model_config_rec%dzstretch_s
  RETURN
END SUBROUTINE nl_get_dzstretch_s
SUBROUTINE nl_get_dzstretch_u ( id_id , dzstretch_u )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: dzstretch_u
  INTEGER id_id
  dzstretch_u = model_config_rec%dzstretch_u
  RETURN
END SUBROUTINE nl_get_dzstretch_u
SUBROUTINE nl_get_ocean_levels ( id_id , ocean_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ocean_levels
  INTEGER id_id
  ocean_levels = model_config_rec%ocean_levels
  RETURN
END SUBROUTINE nl_get_ocean_levels
SUBROUTINE nl_get_ocean_z ( id_id , ocean_z )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: ocean_z
  INTEGER id_id
  ocean_z = model_config_rec%ocean_z(id_id)
  RETURN
END SUBROUTINE nl_get_ocean_z
SUBROUTINE nl_get_ocean_t ( id_id , ocean_t )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: ocean_t
  INTEGER id_id
  ocean_t = model_config_rec%ocean_t(id_id)
  RETURN
END SUBROUTINE nl_get_ocean_t
SUBROUTINE nl_get_ocean_s ( id_id , ocean_s )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: ocean_s
  INTEGER id_id
  ocean_s = model_config_rec%ocean_s(id_id)
  RETURN
END SUBROUTINE nl_get_ocean_s
SUBROUTINE nl_get_num_traj ( id_id , num_traj )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_traj
  INTEGER id_id
  num_traj = model_config_rec%num_traj
  RETURN
END SUBROUTINE nl_get_num_traj
SUBROUTINE nl_get_max_ts_level ( id_id , max_ts_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: max_ts_level
  INTEGER id_id
  max_ts_level = model_config_rec%max_ts_level
  RETURN
END SUBROUTINE nl_get_max_ts_level
SUBROUTINE nl_get_track_loc_in ( id_id , track_loc_in )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: track_loc_in
  INTEGER id_id
  track_loc_in = model_config_rec%track_loc_in
  RETURN
END SUBROUTINE nl_get_track_loc_in
SUBROUTINE nl_get_num_ext_model_couple_dom ( id_id , num_ext_model_couple_dom )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_ext_model_couple_dom
  INTEGER id_id
  num_ext_model_couple_dom = model_config_rec%num_ext_model_couple_dom
  RETURN
END SUBROUTINE nl_get_num_ext_model_couple_dom
SUBROUTINE nl_get_insert_bogus_storm ( id_id , insert_bogus_storm )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: insert_bogus_storm
  INTEGER id_id
  insert_bogus_storm = model_config_rec%insert_bogus_storm
  RETURN
END SUBROUTINE nl_get_insert_bogus_storm
SUBROUTINE nl_get_remove_storm ( id_id , remove_storm )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: remove_storm
  INTEGER id_id
  remove_storm = model_config_rec%remove_storm
  RETURN
END SUBROUTINE nl_get_remove_storm
SUBROUTINE nl_get_num_storm ( id_id , num_storm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_storm
  INTEGER id_id
  num_storm = model_config_rec%num_storm
  RETURN
END SUBROUTINE nl_get_num_storm
SUBROUTINE nl_get_latc_loc ( id_id , latc_loc )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: latc_loc
  INTEGER id_id
  latc_loc = model_config_rec%latc_loc(id_id)
  RETURN
END SUBROUTINE nl_get_latc_loc
SUBROUTINE nl_get_lonc_loc ( id_id , lonc_loc )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: lonc_loc
  INTEGER id_id
  lonc_loc = model_config_rec%lonc_loc(id_id)
  RETURN
END SUBROUTINE nl_get_lonc_loc
SUBROUTINE nl_get_vmax_meters_per_second ( id_id , vmax_meters_per_second )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: vmax_meters_per_second
  INTEGER id_id
  vmax_meters_per_second = model_config_rec%vmax_meters_per_second(id_id)
  RETURN
END SUBROUTINE nl_get_vmax_meters_per_second
SUBROUTINE nl_get_rmax ( id_id , rmax )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: rmax
  INTEGER id_id
  rmax = model_config_rec%rmax(id_id)
  RETURN
END SUBROUTINE nl_get_rmax
SUBROUTINE nl_get_vmax_ratio ( id_id , vmax_ratio )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: vmax_ratio
  INTEGER id_id
  vmax_ratio = model_config_rec%vmax_ratio(id_id)
  RETURN
END SUBROUTINE nl_get_vmax_ratio
SUBROUTINE nl_get_rankine_lid ( id_id , rankine_lid )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: rankine_lid
  INTEGER id_id
  rankine_lid = model_config_rec%rankine_lid
  RETURN
END SUBROUTINE nl_get_rankine_lid
SUBROUTINE nl_get_physics_suite ( id_id , physics_suite )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: physics_suite
  INTEGER id_id
  physics_suite = trim(model_config_rec%physics_suite)
  RETURN
END SUBROUTINE nl_get_physics_suite
SUBROUTINE nl_get_force_read_thompson ( id_id , force_read_thompson )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: force_read_thompson
  INTEGER id_id
  force_read_thompson = model_config_rec%force_read_thompson
  RETURN
END SUBROUTINE nl_get_force_read_thompson
SUBROUTINE nl_get_write_thompson_tables ( id_id , write_thompson_tables )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: write_thompson_tables
  INTEGER id_id
  write_thompson_tables = model_config_rec%write_thompson_tables
  RETURN
END SUBROUTINE nl_get_write_thompson_tables
SUBROUTINE nl_get_mp_physics ( id_id , mp_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mp_physics
  INTEGER id_id
  mp_physics = model_config_rec%mp_physics(id_id)
  RETURN
END SUBROUTINE nl_get_mp_physics
SUBROUTINE nl_get_nssl_cccn ( id_id , nssl_cccn )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_cccn
  INTEGER id_id
  nssl_cccn = model_config_rec%nssl_cccn(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cccn
SUBROUTINE nl_get_nssl_alphah ( id_id , nssl_alphah )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_alphah
  INTEGER id_id
  nssl_alphah = model_config_rec%nssl_alphah(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_alphah
SUBROUTINE nl_get_nssl_alphahl ( id_id , nssl_alphahl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_alphahl
  INTEGER id_id
  nssl_alphahl = model_config_rec%nssl_alphahl(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_alphahl
SUBROUTINE nl_get_nssl_cnoh ( id_id , nssl_cnoh )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_cnoh
  INTEGER id_id
  nssl_cnoh = model_config_rec%nssl_cnoh(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnoh
SUBROUTINE nl_get_nssl_cnohl ( id_id , nssl_cnohl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_cnohl
  INTEGER id_id
  nssl_cnohl = model_config_rec%nssl_cnohl(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnohl
SUBROUTINE nl_get_nssl_cnor ( id_id , nssl_cnor )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_cnor
  INTEGER id_id
  nssl_cnor = model_config_rec%nssl_cnor(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnor
SUBROUTINE nl_get_nssl_cnos ( id_id , nssl_cnos )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_cnos
  INTEGER id_id
  nssl_cnos = model_config_rec%nssl_cnos(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_cnos
SUBROUTINE nl_get_nssl_rho_qh ( id_id , nssl_rho_qh )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_rho_qh
  INTEGER id_id
  nssl_rho_qh = model_config_rec%nssl_rho_qh(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_rho_qh
SUBROUTINE nl_get_nssl_rho_qhl ( id_id , nssl_rho_qhl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_rho_qhl
  INTEGER id_id
  nssl_rho_qhl = model_config_rec%nssl_rho_qhl(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_rho_qhl
SUBROUTINE nl_get_nssl_rho_qs ( id_id , nssl_rho_qs )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: nssl_rho_qs
  INTEGER id_id
  nssl_rho_qs = model_config_rec%nssl_rho_qs(id_id)
  RETURN
END SUBROUTINE nl_get_nssl_rho_qs
SUBROUTINE nl_get_ccnty ( id_id , ccnty )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ccnty
  INTEGER id_id
  ccnty = model_config_rec%ccnty
  RETURN
END SUBROUTINE nl_get_ccnty
SUBROUTINE nl_get_nudge_lightning ( id_id , nudge_lightning )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nudge_lightning
  INTEGER id_id
  nudge_lightning = model_config_rec%nudge_lightning(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_lightning
SUBROUTINE nl_get_nudge_light_times ( id_id , nudge_light_times )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nudge_light_times
  INTEGER id_id
  nudge_light_times = model_config_rec%nudge_light_times(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_light_times
SUBROUTINE nl_get_nudge_light_timee ( id_id , nudge_light_timee )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nudge_light_timee
  INTEGER id_id
  nudge_light_timee = model_config_rec%nudge_light_timee(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_light_timee
SUBROUTINE nl_get_nudge_light_int ( id_id , nudge_light_int )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nudge_light_int
  INTEGER id_id
  nudge_light_int = model_config_rec%nudge_light_int(id_id)
  RETURN
END SUBROUTINE nl_get_nudge_light_int
SUBROUTINE nl_get_path_to_files ( id_id , path_to_files )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(OUT) :: path_to_files
  INTEGER id_id
  path_to_files = trim(model_config_rec%path_to_files)
  RETURN
END SUBROUTINE nl_get_path_to_files
SUBROUTINE nl_get_gsfcgce_hail ( id_id , gsfcgce_hail )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gsfcgce_hail
  INTEGER id_id
  gsfcgce_hail = model_config_rec%gsfcgce_hail
  RETURN
END SUBROUTINE nl_get_gsfcgce_hail
SUBROUTINE nl_get_gsfcgce_2ice ( id_id , gsfcgce_2ice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: gsfcgce_2ice
  INTEGER id_id
  gsfcgce_2ice = model_config_rec%gsfcgce_2ice
  RETURN
END SUBROUTINE nl_get_gsfcgce_2ice
SUBROUTINE nl_get_progn ( id_id , progn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: progn
  INTEGER id_id
  progn = model_config_rec%progn(id_id)
  RETURN
END SUBROUTINE nl_get_progn
SUBROUTINE nl_get_accum_mode ( id_id , accum_mode )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: accum_mode
  INTEGER id_id
  accum_mode = model_config_rec%accum_mode
  RETURN
END SUBROUTINE nl_get_accum_mode
SUBROUTINE nl_get_aitken_mode ( id_id , aitken_mode )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: aitken_mode
  INTEGER id_id
  aitken_mode = model_config_rec%aitken_mode
  RETURN
END SUBROUTINE nl_get_aitken_mode
SUBROUTINE nl_get_coarse_mode ( id_id , coarse_mode )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: coarse_mode
  INTEGER id_id
  coarse_mode = model_config_rec%coarse_mode
  RETURN
END SUBROUTINE nl_get_coarse_mode
SUBROUTINE nl_get_do_radar_ref ( id_id , do_radar_ref )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: do_radar_ref
  INTEGER id_id
  do_radar_ref = model_config_rec%do_radar_ref
  RETURN
END SUBROUTINE nl_get_do_radar_ref
SUBROUTINE nl_get_compute_radar_ref ( id_id , compute_radar_ref )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: compute_radar_ref
  INTEGER id_id
  compute_radar_ref = model_config_rec%compute_radar_ref
  RETURN
END SUBROUTINE nl_get_compute_radar_ref
SUBROUTINE nl_get_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ra_lw_physics
  INTEGER id_id
  ra_lw_physics = model_config_rec%ra_lw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_lw_physics
SUBROUTINE nl_get_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ra_sw_physics
  INTEGER id_id
  ra_sw_physics = model_config_rec%ra_sw_physics(id_id)
  RETURN
END SUBROUTINE nl_get_ra_sw_physics
SUBROUTINE nl_get_ra_sw_eclipse ( id_id , ra_sw_eclipse )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ra_sw_eclipse
  INTEGER id_id
  ra_sw_eclipse = model_config_rec%ra_sw_eclipse
  RETURN
END SUBROUTINE nl_get_ra_sw_eclipse
SUBROUTINE nl_get_ghg_input ( id_id , ghg_input )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ghg_input
  INTEGER id_id
  ghg_input = model_config_rec%ghg_input
  RETURN
END SUBROUTINE nl_get_ghg_input
SUBROUTINE nl_get_radt ( id_id , radt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: radt
  INTEGER id_id
  radt = model_config_rec%radt(id_id)
  RETURN
END SUBROUTINE nl_get_radt
SUBROUTINE nl_get_naer ( id_id , naer )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: naer
  INTEGER id_id
  naer = model_config_rec%naer(id_id)
  RETURN
END SUBROUTINE nl_get_naer
SUBROUTINE nl_get_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sf_sfclay_physics
  INTEGER id_id
  sf_sfclay_physics = model_config_rec%sf_sfclay_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_sfclay_physics
SUBROUTINE nl_get_sf_surf_irr_scheme ( id_id , sf_surf_irr_scheme )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sf_surf_irr_scheme
  INTEGER id_id
  sf_surf_irr_scheme = model_config_rec%sf_surf_irr_scheme(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surf_irr_scheme
SUBROUTINE nl_get_sf_surf_irr_alloc ( id_id , sf_surf_irr_alloc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sf_surf_irr_alloc
  INTEGER id_id
  sf_surf_irr_alloc = model_config_rec%sf_surf_irr_alloc
  RETURN
END SUBROUTINE nl_get_sf_surf_irr_alloc
SUBROUTINE nl_get_irr_daily_amount ( id_id , irr_daily_amount )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: irr_daily_amount
  INTEGER id_id
  irr_daily_amount = model_config_rec%irr_daily_amount(id_id)
  RETURN
END SUBROUTINE nl_get_irr_daily_amount
SUBROUTINE nl_get_irr_start_hour ( id_id , irr_start_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irr_start_hour
  INTEGER id_id
  irr_start_hour = model_config_rec%irr_start_hour(id_id)
  RETURN
END SUBROUTINE nl_get_irr_start_hour
SUBROUTINE nl_get_irr_num_hours ( id_id , irr_num_hours )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irr_num_hours
  INTEGER id_id
  irr_num_hours = model_config_rec%irr_num_hours(id_id)
  RETURN
END SUBROUTINE nl_get_irr_num_hours
SUBROUTINE nl_get_irr_start_julianday ( id_id , irr_start_julianday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irr_start_julianday
  INTEGER id_id
  irr_start_julianday = model_config_rec%irr_start_julianday(id_id)
  RETURN
END SUBROUTINE nl_get_irr_start_julianday
SUBROUTINE nl_get_irr_end_julianday ( id_id , irr_end_julianday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irr_end_julianday
  INTEGER id_id
  irr_end_julianday = model_config_rec%irr_end_julianday(id_id)
  RETURN
END SUBROUTINE nl_get_irr_end_julianday
SUBROUTINE nl_get_irr_freq ( id_id , irr_freq )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irr_freq
  INTEGER id_id
  irr_freq = model_config_rec%irr_freq(id_id)
  RETURN
END SUBROUTINE nl_get_irr_freq
SUBROUTINE nl_get_irr_ph ( id_id , irr_ph )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: irr_ph
  INTEGER id_id
  irr_ph = model_config_rec%irr_ph(id_id)
  RETURN
END SUBROUTINE nl_get_irr_ph
SUBROUTINE nl_get_sf_surface_physics ( id_id , sf_surface_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sf_surface_physics
  INTEGER id_id
  sf_surface_physics = model_config_rec%sf_surface_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_surface_physics
SUBROUTINE nl_get_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_pbl_physics
  INTEGER id_id
  bl_pbl_physics = model_config_rec%bl_pbl_physics(id_id)
  RETURN
END SUBROUTINE nl_get_bl_pbl_physics
SUBROUTINE nl_get_bl_mynn_tkebudget ( id_id , bl_mynn_tkebudget )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_tkebudget
  INTEGER id_id
  bl_mynn_tkebudget = model_config_rec%bl_mynn_tkebudget(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_tkebudget
SUBROUTINE nl_get_ysu_topdown_pblmix ( id_id , ysu_topdown_pblmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ysu_topdown_pblmix
  INTEGER id_id
  ysu_topdown_pblmix = model_config_rec%ysu_topdown_pblmix
  RETURN
END SUBROUTINE nl_get_ysu_topdown_pblmix
SUBROUTINE nl_get_shinhong_tke_diag ( id_id , shinhong_tke_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: shinhong_tke_diag
  INTEGER id_id
  shinhong_tke_diag = model_config_rec%shinhong_tke_diag(id_id)
  RETURN
END SUBROUTINE nl_get_shinhong_tke_diag
SUBROUTINE nl_get_bl_mynn_tkeadvect ( id_id , bl_mynn_tkeadvect )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: bl_mynn_tkeadvect
  INTEGER id_id
  bl_mynn_tkeadvect = model_config_rec%bl_mynn_tkeadvect(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_tkeadvect
SUBROUTINE nl_get_bl_mynn_cloudpdf ( id_id , bl_mynn_cloudpdf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_cloudpdf
  INTEGER id_id
  bl_mynn_cloudpdf = model_config_rec%bl_mynn_cloudpdf
  RETURN
END SUBROUTINE nl_get_bl_mynn_cloudpdf
SUBROUTINE nl_get_bl_mynn_mixlength ( id_id , bl_mynn_mixlength )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_mixlength
  INTEGER id_id
  bl_mynn_mixlength = model_config_rec%bl_mynn_mixlength
  RETURN
END SUBROUTINE nl_get_bl_mynn_mixlength
SUBROUTINE nl_get_bl_mynn_edmf ( id_id , bl_mynn_edmf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_edmf
  INTEGER id_id
  bl_mynn_edmf = model_config_rec%bl_mynn_edmf(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_edmf
SUBROUTINE nl_get_bl_mynn_edmf_mom ( id_id , bl_mynn_edmf_mom )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_edmf_mom
  INTEGER id_id
  bl_mynn_edmf_mom = model_config_rec%bl_mynn_edmf_mom(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_edmf_mom
SUBROUTINE nl_get_bl_mynn_edmf_tke ( id_id , bl_mynn_edmf_tke )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_edmf_tke
  INTEGER id_id
  bl_mynn_edmf_tke = model_config_rec%bl_mynn_edmf_tke(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_edmf_tke
SUBROUTINE nl_get_bl_mynn_mixscalars ( id_id , bl_mynn_mixscalars )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_mixscalars
  INTEGER id_id
  bl_mynn_mixscalars = model_config_rec%bl_mynn_mixscalars(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_mixscalars
SUBROUTINE nl_get_bl_mynn_output ( id_id , bl_mynn_output )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_output
  INTEGER id_id
  bl_mynn_output = model_config_rec%bl_mynn_output(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_output
SUBROUTINE nl_get_bl_mynn_cloudmix ( id_id , bl_mynn_cloudmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_cloudmix
  INTEGER id_id
  bl_mynn_cloudmix = model_config_rec%bl_mynn_cloudmix(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_cloudmix
SUBROUTINE nl_get_bl_mynn_mixqt ( id_id , bl_mynn_mixqt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bl_mynn_mixqt
  INTEGER id_id
  bl_mynn_mixqt = model_config_rec%bl_mynn_mixqt(id_id)
  RETURN
END SUBROUTINE nl_get_bl_mynn_mixqt
SUBROUTINE nl_get_icloud_bl ( id_id , icloud_bl )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: icloud_bl
  INTEGER id_id
  icloud_bl = model_config_rec%icloud_bl
  RETURN
END SUBROUTINE nl_get_icloud_bl
SUBROUTINE nl_get_mfshconv ( id_id , mfshconv )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mfshconv
  INTEGER id_id
  mfshconv = model_config_rec%mfshconv(id_id)
  RETURN
END SUBROUTINE nl_get_mfshconv
SUBROUTINE nl_get_sf_urban_physics ( id_id , sf_urban_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sf_urban_physics
  INTEGER id_id
  sf_urban_physics = model_config_rec%sf_urban_physics(id_id)
  RETURN
END SUBROUTINE nl_get_sf_urban_physics
SUBROUTINE nl_get_bldt ( id_id , bldt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: bldt
  INTEGER id_id
  bldt = model_config_rec%bldt(id_id)
  RETURN
END SUBROUTINE nl_get_bldt
SUBROUTINE nl_get_cu_physics ( id_id , cu_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cu_physics
  INTEGER id_id
  cu_physics = model_config_rec%cu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_cu_physics
SUBROUTINE nl_get_shcu_physics ( id_id , shcu_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: shcu_physics
  INTEGER id_id
  shcu_physics = model_config_rec%shcu_physics(id_id)
  RETURN
END SUBROUTINE nl_get_shcu_physics
SUBROUTINE nl_get_cu_diag ( id_id , cu_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cu_diag
  INTEGER id_id
  cu_diag = model_config_rec%cu_diag(id_id)
  RETURN
END SUBROUTINE nl_get_cu_diag
SUBROUTINE nl_get_kf_edrates ( id_id , kf_edrates )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kf_edrates
  INTEGER id_id
  kf_edrates = model_config_rec%kf_edrates(id_id)
  RETURN
END SUBROUTINE nl_get_kf_edrates
SUBROUTINE nl_get_kfeta_trigger ( id_id , kfeta_trigger )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: kfeta_trigger
  INTEGER id_id
  kfeta_trigger = model_config_rec%kfeta_trigger
  RETURN
END SUBROUTINE nl_get_kfeta_trigger
SUBROUTINE nl_get_nsas_dx_factor ( id_id , nsas_dx_factor )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: nsas_dx_factor
  INTEGER id_id
  nsas_dx_factor = model_config_rec%nsas_dx_factor
  RETURN
END SUBROUTINE nl_get_nsas_dx_factor
SUBROUTINE nl_get_cudt ( id_id , cudt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: cudt
  INTEGER id_id
  cudt = model_config_rec%cudt(id_id)
  RETURN
END SUBROUTINE nl_get_cudt
SUBROUTINE nl_get_gsmdt ( id_id , gsmdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: gsmdt
  INTEGER id_id
  gsmdt = model_config_rec%gsmdt(id_id)
  RETURN
END SUBROUTINE nl_get_gsmdt
SUBROUTINE nl_get_isfflx ( id_id , isfflx )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: isfflx
  INTEGER id_id
  isfflx = model_config_rec%isfflx
  RETURN
END SUBROUTINE nl_get_isfflx
SUBROUTINE nl_get_ifsnow ( id_id , ifsnow )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ifsnow
  INTEGER id_id
  ifsnow = model_config_rec%ifsnow
  RETURN
END SUBROUTINE nl_get_ifsnow
SUBROUTINE nl_get_icloud ( id_id , icloud )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: icloud
  INTEGER id_id
  icloud = model_config_rec%icloud
  RETURN
END SUBROUTINE nl_get_icloud
SUBROUTINE nl_get_cldovrlp ( id_id , cldovrlp )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cldovrlp
  INTEGER id_id
  cldovrlp = model_config_rec%cldovrlp
  RETURN
END SUBROUTINE nl_get_cldovrlp
SUBROUTINE nl_get_idcor ( id_id , idcor )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: idcor
  INTEGER id_id
  idcor = model_config_rec%idcor
  RETURN
END SUBROUTINE nl_get_idcor
SUBROUTINE nl_get_ideal_xland ( id_id , ideal_xland )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ideal_xland
  INTEGER id_id
  ideal_xland = model_config_rec%ideal_xland
  RETURN
END SUBROUTINE nl_get_ideal_xland
SUBROUTINE nl_get_swrad_scat ( id_id , swrad_scat )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: swrad_scat
  INTEGER id_id
  swrad_scat = model_config_rec%swrad_scat
  RETURN
END SUBROUTINE nl_get_swrad_scat
SUBROUTINE nl_get_surface_input_source ( id_id , surface_input_source )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: surface_input_source
  INTEGER id_id
  surface_input_source = model_config_rec%surface_input_source
  RETURN
END SUBROUTINE nl_get_surface_input_source
SUBROUTINE nl_get_num_soil_layers ( id_id , num_soil_layers )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_soil_layers
  INTEGER id_id
  num_soil_layers = model_config_rec%num_soil_layers
  RETURN
END SUBROUTINE nl_get_num_soil_layers
SUBROUTINE nl_get_num_pft_clm ( id_id , num_pft_clm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_pft_clm
  INTEGER id_id
  num_pft_clm = model_config_rec%num_pft_clm
  RETURN
END SUBROUTINE nl_get_num_pft_clm
SUBROUTINE nl_get_input_pft ( id_id , input_pft )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: input_pft
  INTEGER id_id
  input_pft = model_config_rec%input_pft
  RETURN
END SUBROUTINE nl_get_input_pft
SUBROUTINE nl_get_maxpatch ( id_id , maxpatch )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: maxpatch
  INTEGER id_id
  maxpatch = model_config_rec%maxpatch
  RETURN
END SUBROUTINE nl_get_maxpatch
SUBROUTINE nl_get_num_snow_layers ( id_id , num_snow_layers )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_snow_layers
  INTEGER id_id
  num_snow_layers = model_config_rec%num_snow_layers
  RETURN
END SUBROUTINE nl_get_num_snow_layers
SUBROUTINE nl_get_num_snso_layers ( id_id , num_snso_layers )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_snso_layers
  INTEGER id_id
  num_snso_layers = model_config_rec%num_snso_layers
  RETURN
END SUBROUTINE nl_get_num_snso_layers
SUBROUTINE nl_get_num_urban_ndm ( id_id , num_urban_ndm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_ndm
  INTEGER id_id
  num_urban_ndm = model_config_rec%num_urban_ndm
  RETURN
END SUBROUTINE nl_get_num_urban_ndm
SUBROUTINE nl_get_num_urban_ng ( id_id , num_urban_ng )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_ng
  INTEGER id_id
  num_urban_ng = model_config_rec%num_urban_ng
  RETURN
END SUBROUTINE nl_get_num_urban_ng
SUBROUTINE nl_get_num_urban_nwr ( id_id , num_urban_nwr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_nwr
  INTEGER id_id
  num_urban_nwr = model_config_rec%num_urban_nwr
  RETURN
END SUBROUTINE nl_get_num_urban_nwr
SUBROUTINE nl_get_num_urban_ngb ( id_id , num_urban_ngb )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_ngb
  INTEGER id_id
  num_urban_ngb = model_config_rec%num_urban_ngb
  RETURN
END SUBROUTINE nl_get_num_urban_ngb
SUBROUTINE nl_get_num_urban_nf ( id_id , num_urban_nf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_nf
  INTEGER id_id
  num_urban_nf = model_config_rec%num_urban_nf
  RETURN
END SUBROUTINE nl_get_num_urban_nf
SUBROUTINE nl_get_num_urban_nz ( id_id , num_urban_nz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_nz
  INTEGER id_id
  num_urban_nz = model_config_rec%num_urban_nz
  RETURN
END SUBROUTINE nl_get_num_urban_nz
SUBROUTINE nl_get_num_urban_nbui ( id_id , num_urban_nbui )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_nbui
  INTEGER id_id
  num_urban_nbui = model_config_rec%num_urban_nbui
  RETURN
END SUBROUTINE nl_get_num_urban_nbui
SUBROUTINE nl_get_num_urban_ngr ( id_id , num_urban_ngr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_ngr
  INTEGER id_id
  num_urban_ngr = model_config_rec%num_urban_ngr
  RETURN
END SUBROUTINE nl_get_num_urban_ngr
SUBROUTINE nl_get_urban_map_zrd ( id_id , urban_map_zrd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_zrd
  INTEGER id_id
  urban_map_zrd = model_config_rec%urban_map_zrd
  RETURN
END SUBROUTINE nl_get_urban_map_zrd
SUBROUTINE nl_get_urban_map_zwd ( id_id , urban_map_zwd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_zwd
  INTEGER id_id
  urban_map_zwd = model_config_rec%urban_map_zwd
  RETURN
END SUBROUTINE nl_get_urban_map_zwd
SUBROUTINE nl_get_urban_map_gd ( id_id , urban_map_gd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_gd
  INTEGER id_id
  urban_map_gd = model_config_rec%urban_map_gd
  RETURN
END SUBROUTINE nl_get_urban_map_gd
SUBROUTINE nl_get_urban_map_zd ( id_id , urban_map_zd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_zd
  INTEGER id_id
  urban_map_zd = model_config_rec%urban_map_zd
  RETURN
END SUBROUTINE nl_get_urban_map_zd
SUBROUTINE nl_get_urban_map_zdf ( id_id , urban_map_zdf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_zdf
  INTEGER id_id
  urban_map_zdf = model_config_rec%urban_map_zdf
  RETURN
END SUBROUTINE nl_get_urban_map_zdf
SUBROUTINE nl_get_urban_map_bd ( id_id , urban_map_bd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_bd
  INTEGER id_id
  urban_map_bd = model_config_rec%urban_map_bd
  RETURN
END SUBROUTINE nl_get_urban_map_bd
SUBROUTINE nl_get_urban_map_wd ( id_id , urban_map_wd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_wd
  INTEGER id_id
  urban_map_wd = model_config_rec%urban_map_wd
  RETURN
END SUBROUTINE nl_get_urban_map_wd
SUBROUTINE nl_get_urban_map_gbd ( id_id , urban_map_gbd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_gbd
  INTEGER id_id
  urban_map_gbd = model_config_rec%urban_map_gbd
  RETURN
END SUBROUTINE nl_get_urban_map_gbd
SUBROUTINE nl_get_urban_map_fbd ( id_id , urban_map_fbd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_fbd
  INTEGER id_id
  urban_map_fbd = model_config_rec%urban_map_fbd
  RETURN
END SUBROUTINE nl_get_urban_map_fbd
SUBROUTINE nl_get_urban_map_zgrd ( id_id , urban_map_zgrd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: urban_map_zgrd
  INTEGER id_id
  urban_map_zgrd = model_config_rec%urban_map_zgrd
  RETURN
END SUBROUTINE nl_get_urban_map_zgrd
SUBROUTINE nl_get_num_urban_hi ( id_id , num_urban_hi )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_urban_hi
  INTEGER id_id
  num_urban_hi = model_config_rec%num_urban_hi
  RETURN
END SUBROUTINE nl_get_num_urban_hi
SUBROUTINE nl_get_num_months ( id_id , num_months )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_months
  INTEGER id_id
  num_months = model_config_rec%num_months
  RETURN
END SUBROUTINE nl_get_num_months
SUBROUTINE nl_get_sf_surface_mosaic ( id_id , sf_surface_mosaic )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sf_surface_mosaic
  INTEGER id_id
  sf_surface_mosaic = model_config_rec%sf_surface_mosaic
  RETURN
END SUBROUTINE nl_get_sf_surface_mosaic
SUBROUTINE nl_get_mosaic_cat ( id_id , mosaic_cat )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mosaic_cat
  INTEGER id_id
  mosaic_cat = model_config_rec%mosaic_cat
  RETURN
END SUBROUTINE nl_get_mosaic_cat
SUBROUTINE nl_get_mosaic_cat_soil ( id_id , mosaic_cat_soil )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mosaic_cat_soil
  INTEGER id_id
  mosaic_cat_soil = model_config_rec%mosaic_cat_soil
  RETURN
END SUBROUTINE nl_get_mosaic_cat_soil
SUBROUTINE nl_get_mosaic_lu ( id_id , mosaic_lu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mosaic_lu
  INTEGER id_id
  mosaic_lu = model_config_rec%mosaic_lu
  RETURN
END SUBROUTINE nl_get_mosaic_lu
SUBROUTINE nl_get_mosaic_soil ( id_id , mosaic_soil )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mosaic_soil
  INTEGER id_id
  mosaic_soil = model_config_rec%mosaic_soil
  RETURN
END SUBROUTINE nl_get_mosaic_soil
SUBROUTINE nl_get_flag_sm_adj ( id_id , flag_sm_adj )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: flag_sm_adj
  INTEGER id_id
  flag_sm_adj = model_config_rec%flag_sm_adj
  RETURN
END SUBROUTINE nl_get_flag_sm_adj
SUBROUTINE nl_get_maxiens ( id_id , maxiens )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: maxiens
  INTEGER id_id
  maxiens = model_config_rec%maxiens
  RETURN
END SUBROUTINE nl_get_maxiens
SUBROUTINE nl_get_maxens ( id_id , maxens )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: maxens
  INTEGER id_id
  maxens = model_config_rec%maxens
  RETURN
END SUBROUTINE nl_get_maxens
SUBROUTINE nl_get_maxens2 ( id_id , maxens2 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: maxens2
  INTEGER id_id
  maxens2 = model_config_rec%maxens2
  RETURN
END SUBROUTINE nl_get_maxens2
SUBROUTINE nl_get_maxens3 ( id_id , maxens3 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: maxens3
  INTEGER id_id
  maxens3 = model_config_rec%maxens3
  RETURN
END SUBROUTINE nl_get_maxens3
SUBROUTINE nl_get_ensdim ( id_id , ensdim )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ensdim
  INTEGER id_id
  ensdim = model_config_rec%ensdim
  RETURN
END SUBROUTINE nl_get_ensdim
SUBROUTINE nl_get_cugd_avedx ( id_id , cugd_avedx )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cugd_avedx
  INTEGER id_id
  cugd_avedx = model_config_rec%cugd_avedx
  RETURN
END SUBROUTINE nl_get_cugd_avedx
SUBROUTINE nl_get_clos_choice ( id_id , clos_choice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: clos_choice
  INTEGER id_id
  clos_choice = model_config_rec%clos_choice
  RETURN
END SUBROUTINE nl_get_clos_choice
SUBROUTINE nl_get_imomentum ( id_id , imomentum )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: imomentum
  INTEGER id_id
  imomentum = model_config_rec%imomentum
  RETURN
END SUBROUTINE nl_get_imomentum
SUBROUTINE nl_get_ishallow ( id_id , ishallow )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ishallow
  INTEGER id_id
  ishallow = model_config_rec%ishallow
  RETURN
END SUBROUTINE nl_get_ishallow
SUBROUTINE nl_get_convtrans_avglen_m ( id_id , convtrans_avglen_m )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: convtrans_avglen_m
  INTEGER id_id
  convtrans_avglen_m = model_config_rec%convtrans_avglen_m
  RETURN
END SUBROUTINE nl_get_convtrans_avglen_m
SUBROUTINE nl_get_num_land_cat ( id_id , num_land_cat )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_land_cat
  INTEGER id_id
  num_land_cat = model_config_rec%num_land_cat
  RETURN
END SUBROUTINE nl_get_num_land_cat
SUBROUTINE nl_get_use_wudapt_lcz ( id_id , use_wudapt_lcz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: use_wudapt_lcz
  INTEGER id_id
  use_wudapt_lcz = model_config_rec%use_wudapt_lcz
  RETURN
END SUBROUTINE nl_get_use_wudapt_lcz
SUBROUTINE nl_get_num_soil_cat ( id_id , num_soil_cat )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: num_soil_cat
  INTEGER id_id
  num_soil_cat = model_config_rec%num_soil_cat
  RETURN
END SUBROUTINE nl_get_num_soil_cat
SUBROUTINE nl_get_mp_zero_out ( id_id , mp_zero_out )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: mp_zero_out
  INTEGER id_id
  mp_zero_out = model_config_rec%mp_zero_out
  RETURN
END SUBROUTINE nl_get_mp_zero_out
SUBROUTINE nl_get_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: mp_zero_out_thresh
  INTEGER id_id
  mp_zero_out_thresh = model_config_rec%mp_zero_out_thresh
  RETURN
END SUBROUTINE nl_get_mp_zero_out_thresh
SUBROUTINE nl_get_seaice_threshold ( id_id , seaice_threshold )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: seaice_threshold
  INTEGER id_id
  seaice_threshold = model_config_rec%seaice_threshold
  RETURN
END SUBROUTINE nl_get_seaice_threshold
SUBROUTINE nl_get_bmj_rad_feedback ( id_id , bmj_rad_feedback )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: bmj_rad_feedback
  INTEGER id_id
  bmj_rad_feedback = model_config_rec%bmj_rad_feedback(id_id)
  RETURN
END SUBROUTINE nl_get_bmj_rad_feedback
SUBROUTINE nl_get_sst_update ( id_id , sst_update )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sst_update
  INTEGER id_id
  sst_update = model_config_rec%sst_update
  RETURN
END SUBROUTINE nl_get_sst_update
SUBROUTINE nl_get_qna_update ( id_id , qna_update )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: qna_update
  INTEGER id_id
  qna_update = model_config_rec%qna_update
  RETURN
END SUBROUTINE nl_get_qna_update
SUBROUTINE nl_get_sst_skin ( id_id , sst_skin )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sst_skin
  INTEGER id_id
  sst_skin = model_config_rec%sst_skin
  RETURN
END SUBROUTINE nl_get_sst_skin
SUBROUTINE nl_get_tmn_update ( id_id , tmn_update )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: tmn_update
  INTEGER id_id
  tmn_update = model_config_rec%tmn_update
  RETURN
END SUBROUTINE nl_get_tmn_update
SUBROUTINE nl_get_usemonalb ( id_id , usemonalb )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: usemonalb
  INTEGER id_id
  usemonalb = model_config_rec%usemonalb
  RETURN
END SUBROUTINE nl_get_usemonalb
SUBROUTINE nl_get_rdmaxalb ( id_id , rdmaxalb )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: rdmaxalb
  INTEGER id_id
  rdmaxalb = model_config_rec%rdmaxalb
  RETURN
END SUBROUTINE nl_get_rdmaxalb
SUBROUTINE nl_get_rdlai2d ( id_id , rdlai2d )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: rdlai2d
  INTEGER id_id
  rdlai2d = model_config_rec%rdlai2d
  RETURN
END SUBROUTINE nl_get_rdlai2d
SUBROUTINE nl_get_ua_phys ( id_id , ua_phys )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: ua_phys
  INTEGER id_id
  ua_phys = model_config_rec%ua_phys
  RETURN
END SUBROUTINE nl_get_ua_phys
SUBROUTINE nl_get_opt_thcnd ( id_id , opt_thcnd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_thcnd
  INTEGER id_id
  opt_thcnd = model_config_rec%opt_thcnd
  RETURN
END SUBROUTINE nl_get_opt_thcnd
SUBROUTINE nl_get_co2tf ( id_id , co2tf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: co2tf
  INTEGER id_id
  co2tf = model_config_rec%co2tf
  RETURN
END SUBROUTINE nl_get_co2tf
SUBROUTINE nl_get_ra_call_offset ( id_id , ra_call_offset )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: ra_call_offset
  INTEGER id_id
  ra_call_offset = model_config_rec%ra_call_offset
  RETURN
END SUBROUTINE nl_get_ra_call_offset
SUBROUTINE nl_get_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: cam_abs_freq_s
  INTEGER id_id
  cam_abs_freq_s = model_config_rec%cam_abs_freq_s
  RETURN
END SUBROUTINE nl_get_cam_abs_freq_s
SUBROUTINE nl_get_levsiz ( id_id , levsiz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: levsiz
  INTEGER id_id
  levsiz = model_config_rec%levsiz
  RETURN
END SUBROUTINE nl_get_levsiz
SUBROUTINE nl_get_paerlev ( id_id , paerlev )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: paerlev
  INTEGER id_id
  paerlev = model_config_rec%paerlev
  RETURN
END SUBROUTINE nl_get_paerlev
SUBROUTINE nl_get_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cam_abs_dim1
  INTEGER id_id
  cam_abs_dim1 = model_config_rec%cam_abs_dim1
  RETURN
END SUBROUTINE nl_get_cam_abs_dim1
SUBROUTINE nl_get_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: cam_abs_dim2
  INTEGER id_id
  cam_abs_dim2 = model_config_rec%cam_abs_dim2
  RETURN
END SUBROUTINE nl_get_cam_abs_dim2
SUBROUTINE nl_get_lagday ( id_id , lagday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: lagday
  INTEGER id_id
  lagday = model_config_rec%lagday
  RETURN
END SUBROUTINE nl_get_lagday
SUBROUTINE nl_get_no_src_types ( id_id , no_src_types )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: no_src_types
  INTEGER id_id
  no_src_types = model_config_rec%no_src_types
  RETURN
END SUBROUTINE nl_get_no_src_types
SUBROUTINE nl_get_alevsiz ( id_id , alevsiz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: alevsiz
  INTEGER id_id
  alevsiz = model_config_rec%alevsiz
  RETURN
END SUBROUTINE nl_get_alevsiz
SUBROUTINE nl_get_o3input ( id_id , o3input )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: o3input
  INTEGER id_id
  o3input = model_config_rec%o3input
  RETURN
END SUBROUTINE nl_get_o3input
SUBROUTINE nl_get_aer_opt ( id_id , aer_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_opt
  INTEGER id_id
  aer_opt = model_config_rec%aer_opt
  RETURN
END SUBROUTINE nl_get_aer_opt
SUBROUTINE nl_get_swint_opt ( id_id , swint_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: swint_opt
  INTEGER id_id
  swint_opt = model_config_rec%swint_opt
  RETURN
END SUBROUTINE nl_get_swint_opt
SUBROUTINE nl_get_aer_type ( id_id , aer_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_type
  INTEGER id_id
  aer_type = model_config_rec%aer_type(id_id)
  RETURN
END SUBROUTINE nl_get_aer_type
SUBROUTINE nl_get_aer_aod550_opt ( id_id , aer_aod550_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_aod550_opt
  INTEGER id_id
  aer_aod550_opt = model_config_rec%aer_aod550_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_aod550_opt
SUBROUTINE nl_get_aer_angexp_opt ( id_id , aer_angexp_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_angexp_opt
  INTEGER id_id
  aer_angexp_opt = model_config_rec%aer_angexp_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_angexp_opt
SUBROUTINE nl_get_aer_ssa_opt ( id_id , aer_ssa_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_ssa_opt
  INTEGER id_id
  aer_ssa_opt = model_config_rec%aer_ssa_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_ssa_opt
SUBROUTINE nl_get_aer_asy_opt ( id_id , aer_asy_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_asy_opt
  INTEGER id_id
  aer_asy_opt = model_config_rec%aer_asy_opt(id_id)
  RETURN
END SUBROUTINE nl_get_aer_asy_opt
SUBROUTINE nl_get_aer_aod550_val ( id_id , aer_aod550_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: aer_aod550_val
  INTEGER id_id
  aer_aod550_val = model_config_rec%aer_aod550_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_aod550_val
SUBROUTINE nl_get_aer_angexp_val ( id_id , aer_angexp_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: aer_angexp_val
  INTEGER id_id
  aer_angexp_val = model_config_rec%aer_angexp_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_angexp_val
SUBROUTINE nl_get_aer_ssa_val ( id_id , aer_ssa_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: aer_ssa_val
  INTEGER id_id
  aer_ssa_val = model_config_rec%aer_ssa_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_ssa_val
SUBROUTINE nl_get_aer_asy_val ( id_id , aer_asy_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: aer_asy_val
  INTEGER id_id
  aer_asy_val = model_config_rec%aer_asy_val(id_id)
  RETURN
END SUBROUTINE nl_get_aer_asy_val
SUBROUTINE nl_get_cu_rad_feedback ( id_id , cu_rad_feedback )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: cu_rad_feedback
  INTEGER id_id
  cu_rad_feedback = model_config_rec%cu_rad_feedback(id_id)
  RETURN
END SUBROUTINE nl_get_cu_rad_feedback
SUBROUTINE nl_get_dust_emis ( id_id , dust_emis )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dust_emis
  INTEGER id_id
  dust_emis = model_config_rec%dust_emis
  RETURN
END SUBROUTINE nl_get_dust_emis
SUBROUTINE nl_get_erosion_dim ( id_id , erosion_dim )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: erosion_dim
  INTEGER id_id
  erosion_dim = model_config_rec%erosion_dim
  RETURN
END SUBROUTINE nl_get_erosion_dim
SUBROUTINE nl_get_no_src_types_cu ( id_id , no_src_types_cu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: no_src_types_cu
  INTEGER id_id
  no_src_types_cu = model_config_rec%no_src_types_cu
  RETURN
END SUBROUTINE nl_get_no_src_types_cu
SUBROUTINE nl_get_alevsiz_cu ( id_id , alevsiz_cu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: alevsiz_cu
  INTEGER id_id
  alevsiz_cu = model_config_rec%alevsiz_cu
  RETURN
END SUBROUTINE nl_get_alevsiz_cu
SUBROUTINE nl_get_aercu_opt ( id_id , aercu_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aercu_opt
  INTEGER id_id
  aercu_opt = model_config_rec%aercu_opt
  RETURN
END SUBROUTINE nl_get_aercu_opt
SUBROUTINE nl_get_aercu_fct ( id_id , aercu_fct )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: aercu_fct
  INTEGER id_id
  aercu_fct = model_config_rec%aercu_fct
  RETURN
END SUBROUTINE nl_get_aercu_fct
SUBROUTINE nl_get_aercu_used ( id_id , aercu_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aercu_used
  INTEGER id_id
  aercu_used = model_config_rec%aercu_used
  RETURN
END SUBROUTINE nl_get_aercu_used
SUBROUTINE nl_get_couple_farms ( id_id , couple_farms )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: couple_farms
  INTEGER id_id
  couple_farms = model_config_rec%couple_farms
  RETURN
END SUBROUTINE nl_get_couple_farms
SUBROUTINE nl_get_shallowcu_forced_ra ( id_id , shallowcu_forced_ra )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: shallowcu_forced_ra
  INTEGER id_id
  shallowcu_forced_ra = model_config_rec%shallowcu_forced_ra(id_id)
  RETURN
END SUBROUTINE nl_get_shallowcu_forced_ra
SUBROUTINE nl_get_numbins ( id_id , numbins )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: numbins
  INTEGER id_id
  numbins = model_config_rec%numbins(id_id)
  RETURN
END SUBROUTINE nl_get_numbins
SUBROUTINE nl_get_thbinsize ( id_id , thbinsize )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: thbinsize
  INTEGER id_id
  thbinsize = model_config_rec%thbinsize(id_id)
  RETURN
END SUBROUTINE nl_get_thbinsize
SUBROUTINE nl_get_rbinsize ( id_id , rbinsize )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: rbinsize
  INTEGER id_id
  rbinsize = model_config_rec%rbinsize(id_id)
  RETURN
END SUBROUTINE nl_get_rbinsize
SUBROUTINE nl_get_mindeepfreq ( id_id , mindeepfreq )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: mindeepfreq
  INTEGER id_id
  mindeepfreq = model_config_rec%mindeepfreq(id_id)
  RETURN
END SUBROUTINE nl_get_mindeepfreq
SUBROUTINE nl_get_minshallowfreq ( id_id , minshallowfreq )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: minshallowfreq
  INTEGER id_id
  minshallowfreq = model_config_rec%minshallowfreq(id_id)
  RETURN
END SUBROUTINE nl_get_minshallowfreq
SUBROUTINE nl_get_shcu_aerosols_opt ( id_id , shcu_aerosols_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: shcu_aerosols_opt
  INTEGER id_id
  shcu_aerosols_opt = model_config_rec%shcu_aerosols_opt(id_id)
  RETURN
END SUBROUTINE nl_get_shcu_aerosols_opt
SUBROUTINE nl_get_icloud_cu ( id_id , icloud_cu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: icloud_cu
  INTEGER id_id
  icloud_cu = model_config_rec%icloud_cu(id_id)
  RETURN
END SUBROUTINE nl_get_icloud_cu
SUBROUTINE nl_get_pxlsm_smois_init ( id_id , pxlsm_smois_init )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: pxlsm_smois_init
  INTEGER id_id
  pxlsm_smois_init = model_config_rec%pxlsm_smois_init(id_id)
  RETURN
END SUBROUTINE nl_get_pxlsm_smois_init
SUBROUTINE nl_get_pxlsm_modis_veg ( id_id , pxlsm_modis_veg )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: pxlsm_modis_veg
  INTEGER id_id
  pxlsm_modis_veg = model_config_rec%pxlsm_modis_veg(id_id)
  RETURN
END SUBROUTINE nl_get_pxlsm_modis_veg
SUBROUTINE nl_get_omlcall ( id_id , omlcall )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: omlcall
  INTEGER id_id
  omlcall = model_config_rec%omlcall
  RETURN
END SUBROUTINE nl_get_omlcall
SUBROUTINE nl_get_sf_ocean_physics ( id_id , sf_ocean_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: sf_ocean_physics
  INTEGER id_id
  sf_ocean_physics = model_config_rec%sf_ocean_physics
  RETURN
END SUBROUTINE nl_get_sf_ocean_physics
SUBROUTINE nl_get_traj_opt ( id_id , traj_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: traj_opt
  INTEGER id_id
  traj_opt = model_config_rec%traj_opt
  RETURN
END SUBROUTINE nl_get_traj_opt
SUBROUTINE nl_get_dm_has_traj ( id_id , dm_has_traj )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: dm_has_traj
  INTEGER id_id
  dm_has_traj = model_config_rec%dm_has_traj(id_id)
  RETURN
END SUBROUTINE nl_get_dm_has_traj
SUBROUTINE nl_get_tracercall ( id_id , tracercall )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: tracercall
  INTEGER id_id
  tracercall = model_config_rec%tracercall
  RETURN
END SUBROUTINE nl_get_tracercall
SUBROUTINE nl_get_shalwater_z0 ( id_id , shalwater_z0 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: shalwater_z0
  INTEGER id_id
  shalwater_z0 = model_config_rec%shalwater_z0(id_id)
  RETURN
END SUBROUTINE nl_get_shalwater_z0
SUBROUTINE nl_get_shalwater_depth ( id_id , shalwater_depth )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: shalwater_depth
  INTEGER id_id
  shalwater_depth = model_config_rec%shalwater_depth
  RETURN
END SUBROUTINE nl_get_shalwater_depth
SUBROUTINE nl_get_omdt ( id_id , omdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: omdt
  INTEGER id_id
  omdt = model_config_rec%omdt
  RETURN
END SUBROUTINE nl_get_omdt
SUBROUTINE nl_get_oml_hml0 ( id_id , oml_hml0 )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: oml_hml0
  INTEGER id_id
  oml_hml0 = model_config_rec%oml_hml0
  RETURN
END SUBROUTINE nl_get_oml_hml0
SUBROUTINE nl_get_oml_gamma ( id_id , oml_gamma )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: oml_gamma
  INTEGER id_id
  oml_gamma = model_config_rec%oml_gamma
  RETURN
END SUBROUTINE nl_get_oml_gamma
SUBROUTINE nl_get_oml_relaxation_time ( id_id , oml_relaxation_time )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: oml_relaxation_time
  INTEGER id_id
  oml_relaxation_time = model_config_rec%oml_relaxation_time
  RETURN
END SUBROUTINE nl_get_oml_relaxation_time
SUBROUTINE nl_get_isftcflx ( id_id , isftcflx )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: isftcflx
  INTEGER id_id
  isftcflx = model_config_rec%isftcflx
  RETURN
END SUBROUTINE nl_get_isftcflx
SUBROUTINE nl_get_iz0tlnd ( id_id , iz0tlnd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: iz0tlnd
  INTEGER id_id
  iz0tlnd = model_config_rec%iz0tlnd
  RETURN
END SUBROUTINE nl_get_iz0tlnd
SUBROUTINE nl_get_shadlen ( id_id , shadlen )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: shadlen
  INTEGER id_id
  shadlen = model_config_rec%shadlen
  RETURN
END SUBROUTINE nl_get_shadlen
SUBROUTINE nl_get_slope_rad ( id_id , slope_rad )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: slope_rad
  INTEGER id_id
  slope_rad = model_config_rec%slope_rad(id_id)
  RETURN
END SUBROUTINE nl_get_slope_rad
SUBROUTINE nl_get_topo_shading ( id_id , topo_shading )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: topo_shading
  INTEGER id_id
  topo_shading = model_config_rec%topo_shading(id_id)
  RETURN
END SUBROUTINE nl_get_topo_shading
SUBROUTINE nl_get_topo_wind ( id_id , topo_wind )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: topo_wind
  INTEGER id_id
  topo_wind = model_config_rec%topo_wind(id_id)
  RETURN
END SUBROUTINE nl_get_topo_wind
SUBROUTINE nl_get_no_mp_heating ( id_id , no_mp_heating )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: no_mp_heating
  INTEGER id_id
  no_mp_heating = model_config_rec%no_mp_heating
  RETURN
END SUBROUTINE nl_get_no_mp_heating
SUBROUTINE nl_get_fractional_seaice ( id_id , fractional_seaice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: fractional_seaice
  INTEGER id_id
  fractional_seaice = model_config_rec%fractional_seaice
  RETURN
END SUBROUTINE nl_get_fractional_seaice
SUBROUTINE nl_get_seaice_snowdepth_opt ( id_id , seaice_snowdepth_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: seaice_snowdepth_opt
  INTEGER id_id
  seaice_snowdepth_opt = model_config_rec%seaice_snowdepth_opt
  RETURN
END SUBROUTINE nl_get_seaice_snowdepth_opt
SUBROUTINE nl_get_seaice_snowdepth_max ( id_id , seaice_snowdepth_max )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: seaice_snowdepth_max
  INTEGER id_id
  seaice_snowdepth_max = model_config_rec%seaice_snowdepth_max
  RETURN
END SUBROUTINE nl_get_seaice_snowdepth_max
SUBROUTINE nl_get_seaice_snowdepth_min ( id_id , seaice_snowdepth_min )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: seaice_snowdepth_min
  INTEGER id_id
  seaice_snowdepth_min = model_config_rec%seaice_snowdepth_min
  RETURN
END SUBROUTINE nl_get_seaice_snowdepth_min
SUBROUTINE nl_get_seaice_albedo_opt ( id_id , seaice_albedo_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: seaice_albedo_opt
  INTEGER id_id
  seaice_albedo_opt = model_config_rec%seaice_albedo_opt
  RETURN
END SUBROUTINE nl_get_seaice_albedo_opt
SUBROUTINE nl_get_seaice_albedo_default ( id_id , seaice_albedo_default )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: seaice_albedo_default
  INTEGER id_id
  seaice_albedo_default = model_config_rec%seaice_albedo_default
  RETURN
END SUBROUTINE nl_get_seaice_albedo_default
SUBROUTINE nl_get_seaice_thickness_opt ( id_id , seaice_thickness_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: seaice_thickness_opt
  INTEGER id_id
  seaice_thickness_opt = model_config_rec%seaice_thickness_opt
  RETURN
END SUBROUTINE nl_get_seaice_thickness_opt
SUBROUTINE nl_get_seaice_thickness_default ( id_id , seaice_thickness_default )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: seaice_thickness_default
  INTEGER id_id
  seaice_thickness_default = model_config_rec%seaice_thickness_default
  RETURN
END SUBROUTINE nl_get_seaice_thickness_default
SUBROUTINE nl_get_tice2tsk_if2cold ( id_id , tice2tsk_if2cold )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: tice2tsk_if2cold
  INTEGER id_id
  tice2tsk_if2cold = model_config_rec%tice2tsk_if2cold
  RETURN
END SUBROUTINE nl_get_tice2tsk_if2cold
SUBROUTINE nl_get_bucket_mm ( id_id , bucket_mm )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: bucket_mm
  INTEGER id_id
  bucket_mm = model_config_rec%bucket_mm
  RETURN
END SUBROUTINE nl_get_bucket_mm
SUBROUTINE nl_get_bucket_j ( id_id , bucket_j )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: bucket_j
  INTEGER id_id
  bucket_j = model_config_rec%bucket_j
  RETURN
END SUBROUTINE nl_get_bucket_j
SUBROUTINE nl_get_mp_tend_lim ( id_id , mp_tend_lim )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: mp_tend_lim
  INTEGER id_id
  mp_tend_lim = model_config_rec%mp_tend_lim
  RETURN
END SUBROUTINE nl_get_mp_tend_lim
SUBROUTINE nl_get_prec_acc_dt ( id_id , prec_acc_dt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: prec_acc_dt
  INTEGER id_id
  prec_acc_dt = model_config_rec%prec_acc_dt(id_id)
  RETURN
END SUBROUTINE nl_get_prec_acc_dt
SUBROUTINE nl_get_prec_acc_opt ( id_id , prec_acc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: prec_acc_opt
  INTEGER id_id
  prec_acc_opt = model_config_rec%prec_acc_opt
  RETURN
END SUBROUTINE nl_get_prec_acc_opt
SUBROUTINE nl_get_bucketr_opt ( id_id , bucketr_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bucketr_opt
  INTEGER id_id
  bucketr_opt = model_config_rec%bucketr_opt
  RETURN
END SUBROUTINE nl_get_bucketr_opt
SUBROUTINE nl_get_bucketf_opt ( id_id , bucketf_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: bucketf_opt
  INTEGER id_id
  bucketf_opt = model_config_rec%bucketf_opt
  RETURN
END SUBROUTINE nl_get_bucketf_opt
SUBROUTINE nl_get_process_time_series ( id_id , process_time_series )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: process_time_series
  INTEGER id_id
  process_time_series = model_config_rec%process_time_series
  RETURN
END SUBROUTINE nl_get_process_time_series
SUBROUTINE nl_get_grav_settling ( id_id , grav_settling )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: grav_settling
  INTEGER id_id
  grav_settling = model_config_rec%grav_settling(id_id)
  RETURN
END SUBROUTINE nl_get_grav_settling
SUBROUTINE nl_get_sas_pgcon ( id_id , sas_pgcon )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: sas_pgcon
  INTEGER id_id
  sas_pgcon = model_config_rec%sas_pgcon(id_id)
  RETURN
END SUBROUTINE nl_get_sas_pgcon
SUBROUTINE nl_get_scalar_pblmix ( id_id , scalar_pblmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: scalar_pblmix
  INTEGER id_id
  scalar_pblmix = model_config_rec%scalar_pblmix(id_id)
  RETURN
END SUBROUTINE nl_get_scalar_pblmix
SUBROUTINE nl_get_tracer_pblmix ( id_id , tracer_pblmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: tracer_pblmix
  INTEGER id_id
  tracer_pblmix = model_config_rec%tracer_pblmix(id_id)
  RETURN
END SUBROUTINE nl_get_tracer_pblmix
SUBROUTINE nl_get_use_aero_icbc ( id_id , use_aero_icbc )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: use_aero_icbc
  INTEGER id_id
  use_aero_icbc = model_config_rec%use_aero_icbc
  RETURN
END SUBROUTINE nl_get_use_aero_icbc
SUBROUTINE nl_get_use_rap_aero_icbc ( id_id , use_rap_aero_icbc )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: use_rap_aero_icbc
  INTEGER id_id
  use_rap_aero_icbc = model_config_rec%use_rap_aero_icbc
  RETURN
END SUBROUTINE nl_get_use_rap_aero_icbc
SUBROUTINE nl_get_aer_init_opt ( id_id , aer_init_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_init_opt
  INTEGER id_id
  aer_init_opt = model_config_rec%aer_init_opt
  RETURN
END SUBROUTINE nl_get_aer_init_opt
SUBROUTINE nl_get_wif_fire_emit ( id_id , wif_fire_emit )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: wif_fire_emit
  INTEGER id_id
  wif_fire_emit = model_config_rec%wif_fire_emit
  RETURN
END SUBROUTINE nl_get_wif_fire_emit
SUBROUTINE nl_get_aer_fire_emit_opt ( id_id , aer_fire_emit_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: aer_fire_emit_opt
  INTEGER id_id
  aer_fire_emit_opt = model_config_rec%aer_fire_emit_opt
  RETURN
END SUBROUTINE nl_get_aer_fire_emit_opt
SUBROUTINE nl_get_wif_fire_inj ( id_id , wif_fire_inj )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: wif_fire_inj
  INTEGER id_id
  wif_fire_inj = model_config_rec%wif_fire_inj(id_id)
  RETURN
END SUBROUTINE nl_get_wif_fire_inj
SUBROUTINE nl_get_use_mp_re ( id_id , use_mp_re )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: use_mp_re
  INTEGER id_id
  use_mp_re = model_config_rec%use_mp_re
  RETURN
END SUBROUTINE nl_get_use_mp_re
SUBROUTINE nl_get_insert_init_cloud ( id_id , insert_init_cloud )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(OUT) :: insert_init_cloud
  INTEGER id_id
  insert_init_cloud = model_config_rec%insert_init_cloud
  RETURN
END SUBROUTINE nl_get_insert_init_cloud
SUBROUTINE nl_get_ccn_conc ( id_id , ccn_conc )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: ccn_conc
  INTEGER id_id
  ccn_conc = model_config_rec%ccn_conc
  RETURN
END SUBROUTINE nl_get_ccn_conc
SUBROUTINE nl_get_hail_opt ( id_id , hail_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: hail_opt
  INTEGER id_id
  hail_opt = model_config_rec%hail_opt
  RETURN
END SUBROUTINE nl_get_hail_opt
SUBROUTINE nl_get_morr_rimed_ice ( id_id , morr_rimed_ice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: morr_rimed_ice
  INTEGER id_id
  morr_rimed_ice = model_config_rec%morr_rimed_ice
  RETURN
END SUBROUTINE nl_get_morr_rimed_ice
SUBROUTINE nl_get_clean_atm_diag ( id_id , clean_atm_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: clean_atm_diag
  INTEGER id_id
  clean_atm_diag = model_config_rec%clean_atm_diag
  RETURN
END SUBROUTINE nl_get_clean_atm_diag
SUBROUTINE nl_get_calc_clean_atm_diag ( id_id , calc_clean_atm_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: calc_clean_atm_diag
  INTEGER id_id
  calc_clean_atm_diag = model_config_rec%calc_clean_atm_diag
  RETURN
END SUBROUTINE nl_get_calc_clean_atm_diag
SUBROUTINE nl_get_acc_phy_tend ( id_id , acc_phy_tend )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: acc_phy_tend
  INTEGER id_id
  acc_phy_tend = model_config_rec%acc_phy_tend(id_id)
  RETURN
END SUBROUTINE nl_get_acc_phy_tend
SUBROUTINE nl_get_madwrf_opt ( id_id , madwrf_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: madwrf_opt
  INTEGER id_id
  madwrf_opt = model_config_rec%madwrf_opt
  RETURN
END SUBROUTINE nl_get_madwrf_opt
SUBROUTINE nl_get_madwrf_dt_relax ( id_id , madwrf_dt_relax )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: madwrf_dt_relax
  INTEGER id_id
  madwrf_dt_relax = model_config_rec%madwrf_dt_relax
  RETURN
END SUBROUTINE nl_get_madwrf_dt_relax
SUBROUTINE nl_get_madwrf_dt_nudge ( id_id , madwrf_dt_nudge )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: madwrf_dt_nudge
  INTEGER id_id
  madwrf_dt_nudge = model_config_rec%madwrf_dt_nudge
  RETURN
END SUBROUTINE nl_get_madwrf_dt_nudge
SUBROUTINE nl_get_madwrf_cldinit ( id_id , madwrf_cldinit )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: madwrf_cldinit
  INTEGER id_id
  madwrf_cldinit = model_config_rec%madwrf_cldinit
  RETURN
END SUBROUTINE nl_get_madwrf_cldinit
SUBROUTINE nl_get_dveg ( id_id , dveg )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: dveg
  INTEGER id_id
  dveg = model_config_rec%dveg
  RETURN
END SUBROUTINE nl_get_dveg
SUBROUTINE nl_get_opt_crs ( id_id , opt_crs )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_crs
  INTEGER id_id
  opt_crs = model_config_rec%opt_crs
  RETURN
END SUBROUTINE nl_get_opt_crs
SUBROUTINE nl_get_opt_btr ( id_id , opt_btr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_btr
  INTEGER id_id
  opt_btr = model_config_rec%opt_btr
  RETURN
END SUBROUTINE nl_get_opt_btr
SUBROUTINE nl_get_opt_run ( id_id , opt_run )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_run
  INTEGER id_id
  opt_run = model_config_rec%opt_run
  RETURN
END SUBROUTINE nl_get_opt_run
SUBROUTINE nl_get_opt_sfc ( id_id , opt_sfc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_sfc
  INTEGER id_id
  opt_sfc = model_config_rec%opt_sfc
  RETURN
END SUBROUTINE nl_get_opt_sfc
SUBROUTINE nl_get_opt_frz ( id_id , opt_frz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_frz
  INTEGER id_id
  opt_frz = model_config_rec%opt_frz
  RETURN
END SUBROUTINE nl_get_opt_frz
SUBROUTINE nl_get_opt_inf ( id_id , opt_inf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_inf
  INTEGER id_id
  opt_inf = model_config_rec%opt_inf
  RETURN
END SUBROUTINE nl_get_opt_inf
SUBROUTINE nl_get_opt_rad ( id_id , opt_rad )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_rad
  INTEGER id_id
  opt_rad = model_config_rec%opt_rad
  RETURN
END SUBROUTINE nl_get_opt_rad
SUBROUTINE nl_get_opt_alb ( id_id , opt_alb )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_alb
  INTEGER id_id
  opt_alb = model_config_rec%opt_alb
  RETURN
END SUBROUTINE nl_get_opt_alb
SUBROUTINE nl_get_opt_snf ( id_id , opt_snf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_snf
  INTEGER id_id
  opt_snf = model_config_rec%opt_snf
  RETURN
END SUBROUTINE nl_get_opt_snf
SUBROUTINE nl_get_opt_tbot ( id_id , opt_tbot )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_tbot
  INTEGER id_id
  opt_tbot = model_config_rec%opt_tbot
  RETURN
END SUBROUTINE nl_get_opt_tbot
SUBROUTINE nl_get_opt_stc ( id_id , opt_stc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_stc
  INTEGER id_id
  opt_stc = model_config_rec%opt_stc
  RETURN
END SUBROUTINE nl_get_opt_stc
SUBROUTINE nl_get_opt_gla ( id_id , opt_gla )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_gla
  INTEGER id_id
  opt_gla = model_config_rec%opt_gla
  RETURN
END SUBROUTINE nl_get_opt_gla
SUBROUTINE nl_get_opt_rsf ( id_id , opt_rsf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_rsf
  INTEGER id_id
  opt_rsf = model_config_rec%opt_rsf
  RETURN
END SUBROUTINE nl_get_opt_rsf
SUBROUTINE nl_get_opt_soil ( id_id , opt_soil )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_soil
  INTEGER id_id
  opt_soil = model_config_rec%opt_soil
  RETURN
END SUBROUTINE nl_get_opt_soil
SUBROUTINE nl_get_opt_pedo ( id_id , opt_pedo )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_pedo
  INTEGER id_id
  opt_pedo = model_config_rec%opt_pedo
  RETURN
END SUBROUTINE nl_get_opt_pedo
SUBROUTINE nl_get_opt_crop ( id_id , opt_crop )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_crop
  INTEGER id_id
  opt_crop = model_config_rec%opt_crop
  RETURN
END SUBROUTINE nl_get_opt_crop
SUBROUTINE nl_get_opt_irr ( id_id , opt_irr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_irr
  INTEGER id_id
  opt_irr = model_config_rec%opt_irr
  RETURN
END SUBROUTINE nl_get_opt_irr
SUBROUTINE nl_get_opt_irrm ( id_id , opt_irrm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_irrm
  INTEGER id_id
  opt_irrm = model_config_rec%opt_irrm
  RETURN
END SUBROUTINE nl_get_opt_irrm
SUBROUTINE nl_get_opt_infdv ( id_id , opt_infdv )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_infdv
  INTEGER id_id
  opt_infdv = model_config_rec%opt_infdv
  RETURN
END SUBROUTINE nl_get_opt_infdv
SUBROUTINE nl_get_opt_tdrn ( id_id , opt_tdrn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(OUT) :: opt_tdrn
  INTEGER id_id
  opt_tdrn = model_config_rec%opt_tdrn
  RETURN
END SUBROUTINE nl_get_opt_tdrn
SUBROUTINE nl_get_soiltstep ( id_id , soiltstep )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: soiltstep
  INTEGER id_id
  soiltstep = model_config_rec%soiltstep
  RETURN
END SUBROUTINE nl_get_soiltstep
SUBROUTINE nl_get_wtddt ( id_id , wtddt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(OUT) :: wtddt
  INTEGER id_id
  wtddt = model_config_rec%wtddt(id_id)
  RETURN
END SUBROUTINE nl_get_wtddt
!ENDOFREGISTRYGENERATEDINCLUDE
