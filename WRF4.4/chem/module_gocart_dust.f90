MODULE GOCART_DUST






  USE module_data_gocart_dust

CONTAINS
  subroutine gocart_dust_driver(dt,config_flags,alt,t_phy,u_phy,          &
         v_phy,chem,rho_phy,dz8w,smois,u10,v10,erod,dustin,               &
         isltyp,xland,g,emis_dust,                                        &
         ids,ide, jds,jde, kds,kde,                                       &
         ims,ime, jms,jme, kms,kme,                                       &
         its,ite, jts,jte, kts,kte                                        )
  USE module_configure
  USE module_state_description
  IMPLICIT NONE
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   
   INTEGER,DIMENSION( ims:ime , jms:jme ),                                 &
          INTENT(IN   ) ::                           isltyp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                           chem
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_dust),OPTIONAL,&
         INTENT(INOUT ) ::                           emis_dust
  REAL, DIMENSION( ims:ime, config_flags%num_soil_layers, jms:jme ) ,      &
      INTENT(INOUT) ::                               smois
   REAL,  DIMENSION( ims:ime , jms:jme, 3 )                   ,            &
          INTENT(IN   ) ::    erod
   REAL,  DIMENSION( ims:ime , jms:jme, 5 )                   ,            &
          INTENT(INout   ) ::    dustin
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
          INTENT(IN ) ::    u10,v10,xland 
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN ) ::                                alt,               &
                                                      t_phy,               &
                                                     dz8w,                 &
                                              u_phy,v_phy,rho_phy
 
  REAL, INTENT(IN   ) :: dt,g



  integer :: nmx,i,j,k,imx,jmx,lmx
  integer,dimension (1,1) :: ilwi
  real*8, DIMENSION (1,1,3,1) :: erodin
  real*8, DIMENSION (5) :: tc,bems
  real*8, dimension (1,1) :: w10m,gwet,airden
  real                    :: dz_lowest
  real*8  conver,converi
  conver=1.e-9
  converi=1.e9



  imx=1
  jmx=1
  lmx=1
  nmx=5 
  k=kts
  DO j=jts,jte 
  DO i=its,ite



     if(xland(i,j).lt.1.5)then
     ilwi(1,1)=1
     if(config_flags%chem_opt == 2 .or. config_flags%chem_opt == 11 ) then
        tc(:)=1.e-16*conver
     else
        tc(1)=chem(i,kts,j,p_dust_1)*conver
        tc(2)=chem(i,kts,j,p_dust_2)*conver
        tc(3)=chem(i,kts,j,p_dust_3)*conver
        tc(4)=chem(i,kts,j,p_dust_4)*conver
        tc(5)=chem(i,kts,j,p_dust_5)*conver
     endif
     w10m(1,1)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))



     if(dz8w(i,kts,j).lt.12.)w10m=sqrt(u_phy(i,kts,j)*u_phy(i,kts,j)+v_phy(i,kts,j)*v_phy(i,kts,j))
     erodin(1,1,1,1)=erod(i,j,1)
     erodin(1,1,2,1)=erod(i,j,2)
     erodin(1,1,3,1)=erod(i,j,3)



     gwet(1,1)=smois(i,1,j)/porosity(isltyp(i,j))
     airden(1,1)=rho_phy(i,kts,j)
     dz_lowest = dz8w(i,1,j)

    call source_du( imx,jmx,lmx,nmx, dt, tc, &
                     erodin, ilwi, w10m, gwet, airden, &
                     dz_lowest,bems,config_flags%start_month,g)

    if(config_flags%chem_opt == 2 .or. config_flags%chem_opt == 11 ) then
        dustin(i,j,1:5)=tc(1:5)*converi
    else
        chem(i,kts,j,p_dust_1)=tc(1)*converi  
        chem(i,kts,j,p_dust_2)=tc(2)*converi
        chem(i,kts,j,p_dust_3)=tc(3)*converi
        chem(i,kts,j,p_dust_4)=tc(4)*converi
        chem(i,kts,j,p_dust_5)=tc(5)*converi
    endif
     
     
     
     
     
     emis_dust(i,1,j,p_edust1)=emis_dust(i,1,j,p_edust1)+bems(1)
     emis_dust(i,1,j,p_edust2)=emis_dust(i,1,j,p_edust2)+bems(2)
     emis_dust(i,1,j,p_edust3)=emis_dust(i,1,j,p_edust3)+bems(3)
     emis_dust(i,1,j,p_edust4)=emis_dust(i,1,j,p_edust4)+bems(4)
     emis_dust(i,1,j,p_edust5)=emis_dust(i,1,j,p_edust5)+bems(5)
     endif
  enddo
  enddo


end subroutine gocart_dust_driver

  
  SUBROUTINE source_du( imx,jmx,lmx,nmx, dt1, tc, &
                     erod, ilwi, w10m, gwet, airden, &
                     dz_lowest,bems,month,g0)























  INTEGER,   INTENT(IN)    :: nmx,imx,jmx,lmx
  REAL*8,    INTENT(IN)    :: erod(imx,jmx,ndcls,ndsrc)
  INTEGER,   INTENT(IN)    :: ilwi(imx,jmx),month

  REAL*8,    INTENT(IN)    :: w10m(imx,jmx), gwet(imx,jmx)
  REAL*8,    INTENT(IN)    :: airden(imx,jmx,lmx)
  REAL*8,    INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8,    INTENT(OUT)   :: bems(imx,jmx,nmx) 
  REAL,      INTENT(IN  )  :: dz_lowest

  REAL*8    :: den(nmx), diam(nmx)
  REAL*8    :: u_ts0, u_ts, dsrc, srce
  REAL, intent(in)    :: g0
  REAL    :: rhoa, g,dt1
  INTEGER :: i, j, n, m, k

  

  DO n = 1, nmx
     
     
     den(n) = den_dust(n)*1.0D-3
     diam(n) = 2.0*reff_dust(n)*1.0D2
     g = g0*1.0E2
     
     m = ipoint(n)
     DO k = 1, ndsrc
        
        DO i = 1,imx
           DO j = 1,jmx
              rhoa = airden(i,j,1)*1.0D-3
              u_ts0 = 0.13*1.0D-2*SQRT(den(n)*g*diam(n)/rhoa)* &
                   SQRT(1.0+0.006/den(n)/g/(diam(n))**2.5)/ &
                   SQRT(1.928*(1331.0*(diam(n))**1.56+0.38)**0.092-1.0) 

              
              
              IF (gwet(i,j) < 0.5) THEN  
                 u_ts = MAX(0.0D+0,u_ts0*(1.2D+0+2.0D-1*LOG10(MAX(1.0D-3, gwet(i,j)))))
              ELSE
                 
                 u_ts = 100.0
              END IF
              srce = frac_s(n)*erod(i,j,m,k)   
              IF (ilwi(i,j) == 1 ) THEN
                                    
                 dsrc = ch_dust(n,month)*srce*w10m(i,j)**2 * (w10m(i,j) - u_ts)*dt1 
              ELSE 
                 dsrc = 0.0
              END IF
              IF (dsrc < 0.0) dsrc = 0.0
              
              
              tc(i,j,1,n) = tc(i,j,1,n) + dsrc/dz_lowest/airden(i,j,1)  
              bems(i,j,n) = dsrc                     
           END DO
        END DO
     END DO
  END DO
  
END SUBROUTINE source_du


END MODULE GOCART_DUST
