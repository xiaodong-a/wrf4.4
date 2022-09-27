
































MODULE saprc99_Integrator

 USE saprc99_Parameters
 USE saprc99_Precision
 USE saprc99_JacobianSP

  IMPLICIT NONE
 


  INTEGER, PARAMETER :: ifun=1, ijac=2, istp=3, iacc=4, &
    irej=5, idec=6, isol=7, isng=8, itexit=1, ihexit=2
    

  
  CHARACTER(LEN=50), PARAMETER, DIMENSION(-8:1) :: IERR_NAMES = (/ &
    'Matrix is repeatedly singular                     ', & 
    'Step size too small                               ', & 
    'No of steps exceeds maximum bound                 ', & 
    'Improper tolerance values                         ', & 
    'FacMin/FacMax/FacRej must be positive             ', & 
    'Hmin/Hmax/Hstart must be positive                 ', & 
    'Selected Rosenbrock method not implemented        ', & 
    'Improper value for maximal no of steps            ', & 
    '                                                  ', & 
    'Success                                           ' /) 

CONTAINS

SUBROUTINE  saprc99_INTEGRATE( TIN, TOUT, &
  FIX, VAR,  RCONST, ATOL, RTOL, IRR_WRK,  &
  ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U  )

   USE saprc99_Parameters

   IMPLICIT NONE
   REAL(kind=dp), INTENT(INOUT), DIMENSION(NFIX) :: FIX
   REAL(kind=dp), INTENT(INOUT), DIMENSION(NVAR) :: VAR
   REAL(kind=dp), INTENT(INOUT) :: IRR_WRK(NREACT)
   REAL(kind=dp), INTENT(IN), DIMENSION(NSPEC) :: ATOL, RTOL
   REAL(kind=dp), INTENT(IN), DIMENSION(NREACT) :: RCONST
   REAL(kind=dp), INTENT(IN) :: TIN  
   REAL(kind=dp), INTENT(IN) :: TOUT 
   
   INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
   REAL(kind=dp), INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
   INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
   REAL(kind=dp), INTENT(OUT), OPTIONAL :: RSTATUS_U(20)
   INTEGER,  INTENT(OUT), OPTIONAL :: IERR_U

   REAL(kind=dp)  :: STEPMIN


   INTEGER :: N_stp, N_acc, N_rej, N_sng
   SAVE N_stp, N_acc, N_rej, N_sng
   INTEGER :: i, IERR
   REAL(kind=dp) :: RCNTRL(20), RSTATUS(20)
   INTEGER :: ICNTRL(20), ISTATUS(20)


   ICNTRL(:)  = 0
   RCNTRL(:)  = 0.0_dp
   ISTATUS(:) = 0
   RSTATUS(:) = 0.0_dp

   
   
   IF (PRESENT(ICNTRL_U)) THEN
     WHERE(ICNTRL_U(:) > 0) ICNTRL(:) = ICNTRL_U(:)
   END IF
   IF (PRESENT(RCNTRL_U)) THEN
     WHERE(RCNTRL_U(:) > 0) RCNTRL(:) = RCNTRL_U(:)
   END IF

   CALL saprc99_Rosenbrock(VAR, FIX, RCONST, TIN,TOUT,   &
         ATOL,RTOL,               &
         RCNTRL,ICNTRL,RSTATUS,ISTATUS,IRR_WRK,IERR)

   STEPMIN = RCNTRL(ihexit)
   
   IF (PRESENT(ISTATUS_U)) ISTATUS_U(:) = ISTATUS(:)
   IF (PRESENT(RSTATUS_U)) RSTATUS_U(:) = RSTATUS(:)
   IF (PRESENT(IERR_U))    IERR_U       = IERR

END SUBROUTINE  saprc99_INTEGRATE


SUBROUTINE  saprc99_Rosenbrock(Y, FIX, RCONST, Tstart,Tend, &
           AbsTol,RelTol,            &
           RCNTRL,ICNTRL,RSTATUS,ISTATUS,IRR_WRK,IERR)







































































































  USE saprc99_Parameters

  IMPLICIT NONE


   REAL(kind=dp), INTENT(INOUT) :: Y(NVAR)
   REAL(kind=dp), INTENT(INOUT) :: IRR_WRK(NREACT)
   REAL(kind=dp), INTENT(IN), DIMENSION(NFIX) :: FIX
   REAL(kind=dp), INTENT(IN), DIMENSION(NREACT) :: RCONST
   REAL(kind=dp), INTENT(IN)   :: Tstart,Tend
   REAL(kind=dp), INTENT(IN)   :: AbsTol(NVAR),RelTol(NVAR)
   INTEGER, INTENT(IN)    :: ICNTRL(20)
   REAL(kind=dp), INTENT(IN)   :: RCNTRL(20)
   INTEGER, INTENT(INOUT) :: ISTATUS(20)
   REAL(kind=dp), INTENT(INOUT) :: RSTATUS(20)
   INTEGER, INTENT(OUT)   :: IERR

   INTEGER, PARAMETER :: Smax = 6
   INTEGER  :: Method, ros_S
   REAL(kind=dp), DIMENSION(Smax) :: ros_M, ros_E, ros_Alpha, ros_Gamma
   REAL(kind=dp), DIMENSION(Smax*(Smax-1)/2) :: ros_A, ros_C
   REAL(kind=dp) :: ros_ELO
   LOGICAL, DIMENSION(Smax) :: ros_NewF
   CHARACTER(LEN=12) :: ros_Name


  INTEGER :: Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng



   REAL(kind=dp) :: Roundoff, FacMin, FacMax, FacRej, FacSafe
   REAL(kind=dp) :: Hmin, Hmax, Hstart, Hexit
   REAL(kind=dp) :: Texit
   INTEGER :: i, UplimTol, Max_no_steps
   LOGICAL :: Autonomous, VectorTol

   REAL(kind=dp), PARAMETER :: ZERO = 0.0_dp, ONE  = 1.0_dp
   REAL(kind=dp), PARAMETER :: DeltaMin = 1.0E-5_dp


   Nfun = ISTATUS(ifun)
   Njac = ISTATUS(ijac)
   Nstp = ISTATUS(istp)
   Nacc = ISTATUS(iacc)
   Nrej = ISTATUS(irej)
   Ndec = ISTATUS(idec)
   Nsol = ISTATUS(isol)
   Nsng = ISTATUS(isng)


   Autonomous = .NOT.(ICNTRL(1) == 0)







   IF (ICNTRL(2) == 0) THEN
      VectorTol = .TRUE.
         UplimTol  = NVAR
   ELSE
      VectorTol = .FALSE.
         UplimTol  = 1
   END IF


   IF (ICNTRL(3) == 0) THEN
      Method = 4
   ELSEIF ( (ICNTRL(3) >= 1).AND.(ICNTRL(3) <= 5) ) THEN
      Method = ICNTRL(3)
   ELSE
      PRINT * , 'User-selected Rosenbrock method: ICNTRL(3)=', Method
      CALL saprc99_ros_ErrorMsg(-2,Tstart,ZERO,IERR)
      RETURN
   END IF


   IF (ICNTRL(4) == 0) THEN
      Max_no_steps = 100000
   ELSEIF (ICNTRL(4) > 0) THEN
      Max_no_steps=ICNTRL(4)
   ELSE
      PRINT * ,'User-selected max no. of steps: ICNTRL(4)=',ICNTRL(4)
      CALL saprc99_ros_ErrorMsg(-1,Tstart,ZERO,IERR)
      RETURN
   END IF


   Roundoff = saprc99_WLAMCH('E')


   IF (RCNTRL(1) == ZERO) THEN
      Hmin = ZERO
   ELSEIF (RCNTRL(1) > ZERO) THEN
      Hmin = RCNTRL(1)
   ELSE
      PRINT * , 'User-selected Hmin: RCNTRL(1)=', RCNTRL(1)
      CALL saprc99_ros_ErrorMsg(-3,Tstart,ZERO,IERR)
      RETURN
   END IF

   IF (RCNTRL(2) == ZERO) THEN
      Hmax = ABS(Tend-Tstart)
   ELSEIF (RCNTRL(2) > ZERO) THEN
      Hmax = MIN(ABS(RCNTRL(2)),ABS(Tend-Tstart))
   ELSE
      PRINT * , 'User-selected Hmax: RCNTRL(2)=', RCNTRL(2)
      CALL saprc99_ros_ErrorMsg(-3,Tstart,ZERO,IERR)
      RETURN
   END IF

   IF (RCNTRL(3) == ZERO) THEN
      Hstart = MAX(Hmin,DeltaMin)
   ELSEIF (RCNTRL(3) > ZERO) THEN
      Hstart = MIN(ABS(RCNTRL(3)),ABS(Tend-Tstart))
   ELSE
      PRINT * , 'User-selected Hstart: RCNTRL(3)=', RCNTRL(3)
      CALL saprc99_ros_ErrorMsg(-3,Tstart,ZERO,IERR)
      RETURN
   END IF

   IF (RCNTRL(4) == ZERO) THEN
      FacMin = 0.2_dp
   ELSEIF (RCNTRL(4) > ZERO) THEN
      FacMin = RCNTRL(4)
   ELSE
      PRINT * , 'User-selected FacMin: RCNTRL(4)=', RCNTRL(4)
      CALL saprc99_ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN
   END IF
   IF (RCNTRL(5) == ZERO) THEN
      FacMax = 6.0_dp
   ELSEIF (RCNTRL(5) > ZERO) THEN
      FacMax = RCNTRL(5)
   ELSE
      PRINT * , 'User-selected FacMax: RCNTRL(5)=', RCNTRL(5)
      CALL saprc99_ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN
   END IF

   IF (RCNTRL(6) == ZERO) THEN
      FacRej = 0.1_dp
   ELSEIF (RCNTRL(6) > ZERO) THEN
      FacRej = RCNTRL(6)
   ELSE
      PRINT * , 'User-selected FacRej: RCNTRL(6)=', RCNTRL(6)
      CALL saprc99_ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN
   END IF

   IF (RCNTRL(7) == ZERO) THEN
      FacSafe = 0.9_dp
   ELSEIF (RCNTRL(7) > ZERO) THEN
      FacSafe = RCNTRL(7)
   ELSE
      PRINT * , 'User-selected FacSafe: RCNTRL(7)=', RCNTRL(7)
      CALL saprc99_ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN
   END IF

    DO i=1,UplimTol
      IF ( (AbsTol(i) <= ZERO) .OR. (RelTol(i) <= 10.0_dp*Roundoff) &
         .OR. (RelTol(i) >= 1.0_dp) ) THEN
        PRINT * , ' AbsTol(',i,') = ',AbsTol(i)
        PRINT * , ' RelTol(',i,') = ',RelTol(i)
        CALL saprc99_ros_ErrorMsg(-5,Tstart,ZERO,IERR)
        RETURN
      END IF
    END DO



   SELECT CASE (Method)
     CASE (1)
       CALL saprc99_Ros2(ros_S, ros_A, ros_C, ros_M, ros_E,   &
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (2)
       CALL saprc99_Ros3(ros_S, ros_A, ros_C, ros_M, ros_E,   &
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (3)
       CALL saprc99_Ros4(ros_S, ros_A, ros_C, ros_M, ros_E,   &
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (4)
       CALL saprc99_Rodas3(ros_S, ros_A, ros_C, ros_M, ros_E, &
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (5)
       CALL saprc99_Rodas4(ros_S, ros_A, ros_C, ros_M, ros_E, &
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE DEFAULT
       PRINT * , 'Unknown Rosenbrock method: ICNTRL(4)=', Method
       CALL saprc99_ros_ErrorMsg(-2,Tstart,ZERO,IERR)
       RETURN
   END SELECT


   CALL saprc99_ros_Integrator(Y,Tstart,Tend,Texit,      &
        AbsTol, RelTol,                          &

        ros_S, ros_M, ros_E, ros_A, ros_C,       &
        ros_Alpha, ros_Gamma, ros_ELO, ros_NewF, &

        Autonomous, VectorTol, Max_no_steps,     &
        Roundoff, Hmin, Hmax, Hstart, Hexit,     &
        FacMin, FacMax, FacRej, FacSafe,         &

        IRR_WRK,IERR,                            &

         Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng,&

         RCONST, FIX &
)



   ISTATUS(ifun) = Nfun
   ISTATUS(ijac) = Njac
   ISTATUS(istp) = Nstp
   ISTATUS(iacc) = Nacc
   ISTATUS(irej) = Nrej
   ISTATUS(idec) = Ndec
   ISTATUS(isol) = Nsol
   ISTATUS(isng) = Nsng

   RSTATUS(itexit) = Texit
   RSTATUS(ihexit) = Hexit


CONTAINS 



 SUBROUTINE  saprc99_ros_ErrorMsg(Code,T,H,IERR)



   USE saprc99_Precision

   REAL(kind=dp), INTENT(IN) :: T, H
   INTEGER, INTENT(IN)  :: Code
   INTEGER, INTENT(OUT) :: IERR

   IERR = Code
   PRINT * , &
     'Forced exit from Rosenbrock due to the following error:'
   IF ((Code>=-8).AND.(Code<=-1)) THEN
     PRINT *, IERR_NAMES(Code)
   ELSE
     PRINT *, 'Unknown Error code: ', Code
   ENDIF

   PRINT *, "T=", T, "and H=", H

 END SUBROUTINE  saprc99_ros_ErrorMsg


 SUBROUTINE  saprc99_ros_Integrator (Y, Tstart, Tend, T,     &
        AbsTol, RelTol,                          &

        ros_S, ros_M, ros_E, ros_A, ros_C,       &
        ros_Alpha, ros_Gamma, ros_ELO, ros_NewF, &

        Autonomous, VectorTol, Max_no_steps,     &
        Roundoff, Hmin, Hmax, Hstart, Hexit,     &
        FacMin, FacMax, FacRej, FacSafe,         &

        IRR_WRK,IERR,                            &

        Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng, &

        RCONST, FIX )






  IMPLICIT NONE


   REAL(kind=dp), INTENT(INOUT) :: Y(NVAR)

   REAL(kind=dp), INTENT(INOUT) :: IRR_WRK(NREACT)

   REAL(kind=dp), INTENT(IN) :: Tstart,Tend

   REAL(kind=dp), INTENT(OUT) ::  T

   REAL(kind=dp), INTENT(IN) ::  AbsTol(NVAR), RelTol(NVAR)

   INTEGER, INTENT(IN) ::  ros_S
   REAL(kind=dp), INTENT(IN) :: ros_M(ros_S), ros_E(ros_S),  &
       ros_Alpha(ros_S), ros_A(ros_S*(ros_S-1)/2), &
       ros_Gamma(ros_S), ros_C(ros_S*(ros_S-1)/2), ros_ELO
   LOGICAL, INTENT(IN) :: ros_NewF(ros_S)

   LOGICAL, INTENT(IN) :: Autonomous, VectorTol
   REAL(kind=dp), INTENT(IN) :: Hstart, Hmin, Hmax
   INTEGER, INTENT(IN) :: Max_no_steps
   REAL(kind=dp), INTENT(IN) :: Roundoff, FacMin, FacMax, FacRej, FacSafe

   REAL(kind=dp), INTENT(OUT) :: Hexit

   INTEGER, INTENT(OUT) :: IERR

   REAL(kind=dp), INTENT(IN), DIMENSION(NFIX) :: FIX

   REAL(kind=dp), INTENT(IN), DIMENSION(NREACT) :: RCONST


  INTEGER, INTENT(INOUT)  :: Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng


   REAL(kind=dp) :: Ynew(NVAR), Fcn0(NVAR), Fcn(NVAR)
   REAL(kind=dp) :: K(NVAR*ros_S), dFdT(NVAR)
   REAL(kind=dp) :: Jac0(LU_NONZERO), Ghimj(LU_NONZERO)
   REAL(kind=dp) :: H, Hnew, HC, HG, Fac, Tau
   REAL(kind=dp) :: Err, Yerr(NVAR)
   INTEGER :: Pivot(NVAR), Direction, ioffset, j, istage
   LOGICAL :: RejectLastH, RejectMoreH, Singular

   REAL(kind=dp), PARAMETER :: ZERO = 0.0_dp, ONE  = 1.0_dp
   REAL(kind=dp), PARAMETER :: DeltaMin = 1.0E-5_dp







   T = Tstart
   Hexit = 0.0_dp
   H = MIN(Hstart,Hmax)
   IF (ABS(H) <= 10.0_dp*Roundoff) H = DeltaMin

   IF (Tend  >=  Tstart) THEN
     Direction = +1
   ELSE
     Direction = -1
   END IF

   RejectLastH=.FALSE.
   RejectMoreH=.FALSE.



TimeLoop: DO WHILE ( (Direction > 0).AND.((T-Tend)+Roundoff <= ZERO) &
       .OR. (Direction < 0).AND.((Tend-T)+Roundoff <= ZERO) )

   IF ( Nstp > Max_no_steps ) THEN  
      CALL saprc99_ros_ErrorMsg(-6,T,H,IERR)
      RETURN
   END IF
   IF ( ((T+0.1_dp*H) == T).OR.(H <= Roundoff) ) THEN  
      CALL saprc99_ros_ErrorMsg(-7,T,H,IERR)
      RETURN
   END IF


   Hexit = H
   H = MIN(H,ABS(Tend-T))


   CALL saprc99_FunTemplate(T,Y,Fcn0, RCONST, FIX, Nfun)
   IF( T == Tstart ) THEN
     CALL saprc99_IRRFun( Y, FIX, RCONST, IRR_WRK )
   ENDIF


   IF (.NOT.Autonomous) THEN
      CALL saprc99_ros_FunTimeDeriv ( T, Roundoff, Y, &
                Fcn0, dFdT, RCONST, FIX, Nfun )
   END IF


   CALL saprc99_JacTemplate(T,Y,Jac0, FIX, Njac, RCONST)


UntilAccepted: DO

   CALL saprc99_ros_PrepareMatrix(H,Direction,ros_Gamma(1), &
          Jac0,Ghimj,Pivot,Singular, Ndec,  Nsng )
   IF (Singular) THEN 
       CALL saprc99_ros_ErrorMsg(-8,T,H,IERR)
       RETURN
   END IF


Stage: DO istage = 1, ros_S

      
       ioffset = NVAR*(istage-1)

      
       IF ( istage == 1 ) THEN
         CALL saprc99_WCOPY(NVAR,Fcn0,1,Fcn,1)
      
       ELSEIF ( ros_NewF(istage) ) THEN
         CALL saprc99_WCOPY(NVAR,Y,1,Ynew,1)
         DO j = 1, istage-1
           CALL saprc99_WAXPY(NVAR,ros_A((istage-1)*(istage-2)/2+j), &
            K(NVAR*(j-1)+1),1,Ynew,1)
         END DO
         Tau = T + ros_Alpha(istage)*Direction*H
         CALL saprc99_FunTemplate(Tau,Ynew,Fcn, RCONST, FIX, Nfun)
       END IF 
       CALL saprc99_WCOPY(NVAR,Fcn,1,K(ioffset+1),1)
       DO j = 1, istage-1
         HC = ros_C((istage-1)*(istage-2)/2+j)/(Direction*H)
         CALL saprc99_WAXPY(NVAR,HC,K(NVAR*(j-1)+1),1,K(ioffset+1),1)
       END DO
       IF ((.NOT. Autonomous).AND.(ros_Gamma(istage).NE.ZERO)) THEN
         HG = Direction*H*ros_Gamma(istage)
         CALL saprc99_WAXPY(NVAR,HG,dFdT,1,K(ioffset+1),1)
       END IF
       CALL saprc99_ros_Solve(Ghimj, Pivot, K(ioffset+1), Nsol)

   END DO Stage



   CALL saprc99_WCOPY(NVAR,Y,1,Ynew,1)
   DO j=1,ros_S
         CALL saprc99_WAXPY(NVAR,ros_M(j),K(NVAR*(j-1)+1),1,Ynew,1)
   END DO


   CALL saprc99_WSCAL(NVAR,ZERO,Yerr,1)
   DO j=1,ros_S
        CALL saprc99_WAXPY(NVAR,ros_E(j),K(NVAR*(j-1)+1),1,Yerr,1)
   END DO
   Err = saprc99_ros_ErrorNorm ( Y, Ynew, Yerr, AbsTol, RelTol, VectorTol )


   Fac  = MIN(FacMax,MAX(FacMin,FacSafe/Err**(ONE/ros_ELO)))
   Hnew = H*Fac


   Nstp = Nstp+1
   IF ( (Err <= ONE).OR.(H <= Hmin) ) THEN  
      Nacc = Nacc+1
      CALL saprc99_WCOPY(NVAR,Ynew,1,Y,1)
      T = T + Direction*H
      Hnew = MAX(Hmin,MIN(Hnew,Hmax))
      IF (RejectLastH) THEN  
         Hnew = MIN(Hnew,H)
      END IF
      RejectLastH = .FALSE.
      RejectMoreH = .FALSE.
      H = Hnew
      EXIT UntilAccepted 
   ELSE           
      IF (RejectMoreH) THEN
         Hnew = H*FacRej
      END IF
      RejectMoreH = RejectLastH
      RejectLastH = .TRUE.
      H = Hnew
      IF (Nacc >= 1) THEN
         Nrej = Nrej+1
      END IF
   END IF 

   END DO UntilAccepted

   END DO TimeLoop


   IERR = 1  

  END SUBROUTINE  saprc99_ros_Integrator



  REAL(kind=dp) FUNCTION  saprc99_ros_ErrorNorm ( Y, Ynew, Yerr, &
               AbsTol, RelTol, VectorTol )



   IMPLICIT NONE


   REAL(kind=dp), INTENT(IN) :: Y(NVAR), Ynew(NVAR), &
          Yerr(NVAR), AbsTol(NVAR), RelTol(NVAR)
   LOGICAL, INTENT(IN) ::  VectorTol

   REAL(kind=dp) :: Err, Scale, Ymax
   INTEGER  :: i
   REAL(kind=dp), PARAMETER :: ZERO = 0.0_dp

   Err = ZERO
   DO i=1,NVAR
     Ymax = MAX(ABS(Y(i)),ABS(Ynew(i)))
     IF (VectorTol) THEN
       Scale = AbsTol(i)+RelTol(i)*Ymax
     ELSE
       Scale = AbsTol(1)+RelTol(1)*Ymax
     END IF
     Err = Err+(Yerr(i)/Scale)**2
   END DO
   Err  = SQRT(Err/NVAR)

    saprc99_ros_ErrorNorm = Err

  END FUNCTION  saprc99_ros_ErrorNorm



  SUBROUTINE saprc99_ros_FunTimeDeriv ( T, Roundoff, Y, &
                Fcn0, dFdT, RCONST, FIX, Nfun )



   IMPLICIT NONE


   REAL(kind=dp), INTENT(IN) :: T, Roundoff, Y(NVAR), Fcn0(NVAR)
   REAL(kind=dp), INTENT(IN) :: RCONST(NREACT), FIX(NFIX)

   REAL(kind=dp), INTENT(OUT) :: dFdT(NVAR)

   INTEGER, INTENT(INOUT) ::Nfun

   REAL(kind=dp) :: Delta
   REAL(kind=dp), PARAMETER :: ONE = 1.0_dp, DeltaMin = 1.0E-6_dp

   Delta = SQRT(Roundoff)*MAX(DeltaMin,ABS(T))
   CALL saprc99_FunTemplate(T+Delta,Y,dFdT, RCONST, FIX, Nfun)
   CALL saprc99_WAXPY(NVAR,(-ONE),Fcn0,1,dFdT,1)
   CALL saprc99_WSCAL(NVAR,(ONE/Delta),dFdT,1)

  END SUBROUTINE  saprc99_ros_FunTimeDeriv



  SUBROUTINE  saprc99_ros_PrepareMatrix ( H, Direction, gam, &
             Jac0, Ghimj, Pivot, Singular, Ndec,  Nsng  )








   IMPLICIT NONE


   REAL(kind=dp), INTENT(IN) ::  Jac0(LU_NONZERO)
   REAL(kind=dp), INTENT(IN) ::  gam
   INTEGER, INTENT(IN) ::  Direction

   REAL(kind=dp), INTENT(OUT) :: Ghimj(LU_NONZERO)
   LOGICAL, INTENT(OUT) ::  Singular
   INTEGER, INTENT(OUT) ::  Pivot(NVAR)

   REAL(kind=dp), INTENT(INOUT) :: H   
   INTEGER, INTENT(INOUT) ::  Ndec, Nsng

   INTEGER  :: i, ising, Nconsecutive
   REAL(kind=dp) :: ghinv
   REAL(kind=dp), PARAMETER :: ONE  = 1.0_dp, HALF = 0.5_dp

   Nconsecutive = 0
   Singular = .TRUE.

   DO WHILE (Singular)


     CALL saprc99_WCOPY(LU_NONZERO,Jac0,1,Ghimj,1)
     CALL saprc99_WSCAL(LU_NONZERO,(-ONE),Ghimj,1)
     ghinv = ONE/(Direction*H*gam)
     DO i=1,NVAR
       Ghimj(LU_DIAG(i)) = Ghimj(LU_DIAG(i))+ghinv
     END DO

     CALL saprc99_ros_Decomp( Ghimj, Pivot, ising, Ndec )
     IF (ising == 0) THEN

        Singular = .FALSE.
     ELSE 

        Nsng = Nsng+1
        Nconsecutive = Nconsecutive+1
        Singular = .TRUE.
        PRINT*,'Warning: LU Decomposition returned ising = ',ising
        IF (Nconsecutive <= 5) THEN 
           H = H*HALF
        ELSE  
           RETURN
        END IF  
      END IF    

   END DO 

  END SUBROUTINE  saprc99_ros_PrepareMatrix



  SUBROUTINE  saprc99_ros_Decomp( A, Pivot, ising, Ndec )



   IMPLICIT NONE

   REAL(kind=dp), INTENT(INOUT) :: A(LU_NONZERO)

   INTEGER, INTENT(OUT) :: Pivot(NVAR), ising
   INTEGER, INTENT(INOUT) :: Ndec 



CALL decomp_saprc99 ( A, ising )
   Pivot(1) = 1
   Ndec = Ndec + 1

  END SUBROUTINE  saprc99_ros_Decomp



  SUBROUTINE  saprc99_ros_Solve( A, Pivot, b, Nsol )



   IMPLICIT NONE

   REAL(kind=dp), INTENT(IN) :: A(LU_NONZERO)
   INTEGER, INTENT(IN) :: Pivot(NVAR)

   INTEGER, INTENT(INOUT) :: nsol 

   REAL(kind=dp), INTENT(INOUT) :: b(NVAR)


   CALL saprc99_KppSolve( A, b )

   Nsol = Nsol+1

  END SUBROUTINE  saprc99_ros_Solve




  SUBROUTINE  saprc99_Ros2 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
            ros_Gamma,ros_NewF,ros_ELO,ros_Name)




  IMPLICIT NONE

   INTEGER, PARAMETER :: S=2
   INTEGER, INTENT(OUT) ::  ros_S
   REAL(kind=dp), DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   REAL(kind=dp), DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   REAL(kind=dp), INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name

    REAL(kind=dp) :: g

    g = 1.0_dp + 1.0_dp/SQRT(2.0_dp)


    ros_Name = 'ROS-2'

    ros_S = S








    ros_A(1) = (1.0_dp)/g
    ros_C(1) = (-2.0_dp)/g


    ros_NewF(1) = .TRUE.
    ros_NewF(2) = .TRUE.

    ros_M(1)= (3.0_dp)/(2.0_dp*g)
    ros_M(2)= (1.0_dp)/(2.0_dp*g)

    ros_E(1) = 1.0_dp/(2.0_dp*g)
    ros_E(2) = 1.0_dp/(2.0_dp*g)


    ros_ELO = 2.0_dp

    ros_Alpha(1) = 0.0_dp
    ros_Alpha(2) = 1.0_dp

    ros_Gamma(1) = g
    ros_Gamma(2) =-g

 END SUBROUTINE  saprc99_Ros2



  SUBROUTINE  saprc99_Ros3 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
           ros_Gamma,ros_NewF,ros_ELO,ros_Name)




  IMPLICIT NONE

   INTEGER, PARAMETER :: S=3
   INTEGER, INTENT(OUT) ::  ros_S
   REAL(kind=dp), DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   REAL(kind=dp), DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   REAL(kind=dp), INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name


   ros_Name = 'ROS-3'

   ros_S = S








   ros_A(1)= 1.0_dp
   ros_A(2)= 1.0_dp
   ros_A(3)= 0.0_dp

   ros_C(1) = -0.10156171083877702091975600115545E+01_dp
   ros_C(2) =  0.40759956452537699824805835358067E+01_dp
   ros_C(3) =  0.92076794298330791242156818474003E+01_dp


   ros_NewF(1) = .TRUE.
   ros_NewF(2) = .TRUE.
   ros_NewF(3) = .FALSE.

   ros_M(1) =  0.1E+01_dp
   ros_M(2) =  0.61697947043828245592553615689730E+01_dp
   ros_M(3) = -0.42772256543218573326238373806514E+00_dp

   ros_E(1) =  0.5E+00_dp
   ros_E(2) = -0.29079558716805469821718236208017E+01_dp
   ros_E(3) =  0.22354069897811569627360909276199E+00_dp


   ros_ELO = 3.0_dp

   ros_Alpha(1)= 0.0E+00_dp
   ros_Alpha(2)= 0.43586652150845899941601945119356E+00_dp
   ros_Alpha(3)= 0.43586652150845899941601945119356E+00_dp

   ros_Gamma(1)= 0.43586652150845899941601945119356E+00_dp
   ros_Gamma(2)= 0.24291996454816804366592249683314E+00_dp
   ros_Gamma(3)= 0.21851380027664058511513169485832E+01_dp

  END SUBROUTINE  saprc99_Ros3





  SUBROUTINE  saprc99_Ros4 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
           ros_Gamma,ros_NewF,ros_ELO,ros_Name)










  IMPLICIT NONE

   INTEGER, PARAMETER :: S=4
   INTEGER, INTENT(OUT) ::  ros_S
   REAL(kind=dp), DIMENSION(4), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   REAL(kind=dp), DIMENSION(6), INTENT(OUT) :: ros_A, ros_C
   REAL(kind=dp), INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(4), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name

   REAL(kind=dp) :: g



   ros_Name = 'ROS-4'

   ros_S = S








   ros_A(1) = 0.2000000000000000E+01_dp
   ros_A(2) = 0.1867943637803922E+01_dp
   ros_A(3) = 0.2344449711399156E+00_dp
   ros_A(4) = ros_A(2)
   ros_A(5) = ros_A(3)
   ros_A(6) = 0.0_dp

   ros_C(1) =-0.7137615036412310E+01_dp
   ros_C(2) = 0.2580708087951457E+01_dp
   ros_C(3) = 0.6515950076447975E+00_dp
   ros_C(4) =-0.2137148994382534E+01_dp
   ros_C(5) =-0.3214669691237626E+00_dp
   ros_C(6) =-0.6949742501781779E+00_dp


   ros_NewF(1)  = .TRUE.
   ros_NewF(2)  = .TRUE.
   ros_NewF(3)  = .TRUE.
   ros_NewF(4)  = .FALSE.

   ros_M(1) = 0.2255570073418735E+01_dp
   ros_M(2) = 0.2870493262186792E+00_dp
   ros_M(3) = 0.4353179431840180E+00_dp
   ros_M(4) = 0.1093502252409163E+01_dp

   ros_E(1) =-0.2815431932141155E+00_dp
   ros_E(2) =-0.7276199124938920E-01_dp
   ros_E(3) =-0.1082196201495311E+00_dp
   ros_E(4) =-0.1093502252409163E+01_dp


   ros_ELO  = 4.0_dp

   ros_Alpha(1) = 0.0_dp
   ros_Alpha(2) = 0.1145640000000000E+01_dp
   ros_Alpha(3) = 0.6552168638155900E+00_dp
   ros_Alpha(4) = ros_Alpha(3)

   ros_Gamma(1) = 0.5728200000000000E+00_dp
   ros_Gamma(2) =-0.1769193891319233E+01_dp
   ros_Gamma(3) = 0.7592633437920482E+00_dp
   ros_Gamma(4) =-0.1049021087100450E+00_dp

  END SUBROUTINE  saprc99_Ros4


  SUBROUTINE  saprc99_Rodas3 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
            ros_Gamma,ros_NewF,ros_ELO,ros_Name)




  IMPLICIT NONE

   INTEGER, PARAMETER :: S=4
   INTEGER, INTENT(OUT) ::  ros_S
   REAL(kind=dp), DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   REAL(kind=dp), DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   REAL(kind=dp), INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name

   REAL(kind=dp) :: g


   ros_Name = 'RODAS-3'

   ros_S = S








   ros_A(1) = 0.0E+00_dp
   ros_A(2) = 2.0E+00_dp
   ros_A(3) = 0.0E+00_dp
   ros_A(4) = 2.0E+00_dp
   ros_A(5) = 0.0E+00_dp
   ros_A(6) = 1.0E+00_dp

   ros_C(1) = 4.0E+00_dp
   ros_C(2) = 1.0E+00_dp
   ros_C(3) =-1.0E+00_dp
   ros_C(4) = 1.0E+00_dp
   ros_C(5) =-1.0E+00_dp
   ros_C(6) =-(8.0E+00_dp/3.0E+00_dp)



   ros_NewF(1)  = .TRUE.
   ros_NewF(2)  = .FALSE.
   ros_NewF(3)  = .TRUE.
   ros_NewF(4)  = .TRUE.

   ros_M(1) = 2.0E+00_dp
   ros_M(2) = 0.0E+00_dp
   ros_M(3) = 1.0E+00_dp
   ros_M(4) = 1.0E+00_dp

   ros_E(1) = 0.0E+00_dp
   ros_E(2) = 0.0E+00_dp
   ros_E(3) = 0.0E+00_dp
   ros_E(4) = 1.0E+00_dp


   ros_ELO  = 3.0E+00_dp

   ros_Alpha(1) = 0.0E+00_dp
   ros_Alpha(2) = 0.0E+00_dp
   ros_Alpha(3) = 1.0E+00_dp
   ros_Alpha(4) = 1.0E+00_dp

   ros_Gamma(1) = 0.5E+00_dp
   ros_Gamma(2) = 1.5E+00_dp
   ros_Gamma(3) = 0.0E+00_dp
   ros_Gamma(4) = 0.0E+00_dp

  END SUBROUTINE  saprc99_Rodas3


  SUBROUTINE  saprc99_Rodas4 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
             ros_Gamma,ros_NewF,ros_ELO,ros_Name)









  IMPLICIT NONE

   INTEGER, PARAMETER :: S=6
   INTEGER, INTENT(OUT) ::  ros_S
   REAL(kind=dp), DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   REAL(kind=dp), DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   REAL(kind=dp), INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name

    REAL(kind=dp) :: g


    ros_Name = 'RODAS-4'

    ros_S = 6


    ros_Alpha(1) = 0.000_dp
    ros_Alpha(2) = 0.386_dp
    ros_Alpha(3) = 0.210_dp
    ros_Alpha(4) = 0.630_dp
    ros_Alpha(5) = 1.000_dp
    ros_Alpha(6) = 1.000_dp


    ros_Gamma(1) = 0.2500000000000000E+00_dp
    ros_Gamma(2) =-0.1043000000000000E+00_dp
    ros_Gamma(3) = 0.1035000000000000E+00_dp
    ros_Gamma(4) =-0.3620000000000023E-01_dp
    ros_Gamma(5) = 0.0_dp
    ros_Gamma(6) = 0.0_dp







    ros_A(1) = 0.1544000000000000E+01_dp
    ros_A(2) = 0.9466785280815826E+00_dp
    ros_A(3) = 0.2557011698983284E+00_dp
    ros_A(4) = 0.3314825187068521E+01_dp
    ros_A(5) = 0.2896124015972201E+01_dp
    ros_A(6) = 0.9986419139977817E+00_dp
    ros_A(7) = 0.1221224509226641E+01_dp
    ros_A(8) = 0.6019134481288629E+01_dp
    ros_A(9) = 0.1253708332932087E+02_dp
    ros_A(10) =-0.6878860361058950E+00_dp
    ros_A(11) = ros_A(7)
    ros_A(12) = ros_A(8)
    ros_A(13) = ros_A(9)
    ros_A(14) = ros_A(10)
    ros_A(15) = 1.0E+00_dp

    ros_C(1) =-0.5668800000000000E+01_dp
    ros_C(2) =-0.2430093356833875E+01_dp
    ros_C(3) =-0.2063599157091915E+00_dp
    ros_C(4) =-0.1073529058151375E+00_dp
    ros_C(5) =-0.9594562251023355E+01_dp
    ros_C(6) =-0.2047028614809616E+02_dp
    ros_C(7) = 0.7496443313967647E+01_dp
    ros_C(8) =-0.1024680431464352E+02_dp
    ros_C(9) =-0.3399990352819905E+02_dp
    ros_C(10) = 0.1170890893206160E+02_dp
    ros_C(11) = 0.8083246795921522E+01_dp
    ros_C(12) =-0.7981132988064893E+01_dp
    ros_C(13) =-0.3152159432874371E+02_dp
    ros_C(14) = 0.1631930543123136E+02_dp
    ros_C(15) =-0.6058818238834054E+01_dp


    ros_M(1) = ros_A(7)
    ros_M(2) = ros_A(8)
    ros_M(3) = ros_A(9)
    ros_M(4) = ros_A(10)
    ros_M(5) = 1.0E+00_dp
    ros_M(6) = 1.0E+00_dp


    ros_E(1) = 0.0E+00_dp
    ros_E(2) = 0.0E+00_dp
    ros_E(3) = 0.0E+00_dp
    ros_E(4) = 0.0E+00_dp
    ros_E(5) = 0.0E+00_dp
    ros_E(6) = 1.0E+00_dp



    ros_NewF(1) = .TRUE.
    ros_NewF(2) = .TRUE.
    ros_NewF(3) = .TRUE.
    ros_NewF(4) = .TRUE.
    ros_NewF(5) = .TRUE.
    ros_NewF(6) = .TRUE.



    ros_ELO = 4.0_dp

  END SUBROUTINE  saprc99_Rodas4




END SUBROUTINE  saprc99_Rosenbrock




SUBROUTINE  saprc99_FunTemplate( T, Y, Ydot, RCONST, FIX, Nfun )




   USE saprc99_Parameters




   REAL(kind=dp) :: T, Y(NVAR)
   REAL(kind=dp) :: RCONST(NREACT)
   REAL(kind=dp) :: FIX(NFIX)

   REAL(kind=dp) :: Ydot(NVAR)
   INTEGER :: Nfun









   CALL saprc99_Fun( Y, FIX, RCONST, Ydot )


   Nfun = Nfun+1

END SUBROUTINE  saprc99_FunTemplate



SUBROUTINE  saprc99_JacTemplate( T, Y, Jcb, FIX, Njac, RCONST )




 USE saprc99_Parameters
 
 USE saprc99_Jacobian



    REAL(kind=dp) :: T, Y(NVAR)
    REAL(kind=dp) :: FIX(NFIX)
    REAL(kind=dp) :: RCONST(NREACT)

    INTEGER :: Njac


    REAL(kind=dp) :: Jcb(LU_NONZERO)

    REAL(kind=dp) :: Told





    CALL saprc99_Jac_SP( Y, FIX, RCONST, Jcb )


    Njac = Njac+1

END SUBROUTINE  saprc99_JacTemplate

















SUBROUTINE saprc99_Fun ( V, F, RCT, Vdot )


  REAL(kind=dp) :: V(NVAR)

  REAL(kind=dp) :: F(NFIX)

  REAL(kind=dp) :: RCT(NREACT)

  REAL(kind=dp) :: Vdot(NVAR)




  REAL(kind=dp) :: A(NREACT)


  A(1) = RCT(1)*V(79)
  A(2) = RCT(2)*V(70)*F(2)
  A(3) = RCT(3)*V(70)*V(74)
  A(4) = RCT(4)*V(70)*V(78)*F(2)
  A(5) = RCT(5)*V(70)*V(79)
  A(6) = RCT(6)*V(70)*V(79)
  A(7) = RCT(7)*V(74)*V(78)
  A(8) = RCT(8)*V(74)*V(79)
  A(9) = RCT(9)*V(78)*V(80)
  A(10) = RCT(10)*V(78)*V(78)*F(2)
  A(11) = RCT(11)*V(79)*V(80)
  A(12) = RCT(12)*V(28)
  A(13) = RCT(13)*V(28)*F(1)
  A(14) = RCT(14)*V(79)*V(80)
  A(15) = RCT(15)*V(80)
  A(16) = RCT(16)*V(80)
  A(17) = RCT(17)*V(74)
  A(18) = RCT(18)*V(74)
  A(19) = RCT(19)*V(14)*F(1)
  A(20) = RCT(20)*V(14)*F(2)
  A(21) = RCT(21)*V(78)*V(82)
  A(22) = RCT(22)*V(29)
  A(23) = RCT(23)*V(29)
  A(24) = RCT(24)*V(29)*V(82)
  A(25) = RCT(25)*V(79)*V(82)
  A(26) = RCT(26)*V(80)*V(82)
  A(27) = RCT(27)*V(52)*V(82)
  A(28) = RCT(28)*V(52)
  A(29) = RCT(29)*V(51)*V(82)
  A(30) = RCT(30)*V(74)*V(82)
  A(31) = RCT(31)*V(78)*V(85)
  A(32) = RCT(32)*V(79)*V(85)
  A(33) = RCT(33)*V(38)
  A(34) = RCT(34)*V(38)
  A(35) = RCT(35)*V(38)*V(82)
  A(36) = RCT(36)*V(74)*V(85)
  A(37) = RCT(37)*V(85)*V(85)
  A(38) = RCT(38)*V(85)*V(85)*F(1)
  A(39) = RCT(39)*V(80)*V(85)
  A(40) = RCT(40)*V(80)*V(80)
  A(41) = RCT(41)*V(24)
  A(42) = RCT(42)*V(24)*V(82)
  A(43) = RCT(43)*V(82)*V(85)
  A(44) = RCT(44)*V(16)*V(82)
  A(45) = RCT(45)*V(82)*F(2)
  A(46) = RCT(46)*V(78)*V(81)
  A(47) = RCT(47)*V(81)*V(85)
  A(48) = RCT(48)*V(80)*V(81)
  A(49) = RCT(49)*V(81)*V(81)
  A(50) = RCT(50)*V(81)*V(81)
  A(51) = RCT(51)*V(77)*V(78)
  A(52) = RCT(52)*V(77)*V(85)
  A(53) = RCT(53)*V(77)*V(80)
  A(54) = RCT(54)*V(77)*V(81)
  A(55) = RCT(55)*V(77)*V(77)
  A(56) = RCT(56)*V(60)*V(78)
  A(57) = RCT(57)*V(60)*V(85)
  A(58) = RCT(58)*V(60)*V(80)
  A(59) = RCT(59)*V(60)*V(81)
  A(60) = RCT(60)*V(60)*V(77)
  A(62) = RCT(62)*V(78)*V(86)
  A(63) = RCT(63)*V(85)*V(86)
  A(64) = RCT(64)*V(81)*V(86)
  A(65) = RCT(65)*V(80)*V(86)
  A(66) = RCT(66)*V(77)*V(86)
  A(67) = RCT(67)*V(60)*V(86)
  A(68) = RCT(68)*V(86)*V(86)
  A(69) = RCT(69)*V(76)*V(79)
  A(70) = RCT(70)*V(20)
  A(71) = RCT(71)*V(76)*V(78)
  A(72) = RCT(72)*V(76)*V(85)
  A(73) = RCT(73)*V(76)*V(80)
  A(74) = RCT(74)*V(76)*V(81)
  A(75) = RCT(75)*V(76)*V(77)
  A(76) = RCT(76)*V(60)*V(76)
  A(77) = RCT(77)*V(76)*V(86)
  A(78) = RCT(78)*V(76)*V(76)
  A(79) = RCT(79)*V(79)*V(83)
  A(80) = RCT(80)*V(21)
  A(81) = RCT(81)*V(78)*V(83)
  A(82) = RCT(82)*V(83)*V(85)
  A(83) = RCT(83)*V(80)*V(83)
  A(84) = RCT(84)*V(81)*V(83)
  A(85) = RCT(85)*V(77)*V(83)
  A(86) = RCT(86)*V(60)*V(83)
  A(87) = RCT(87)*V(83)*V(86)
  A(88) = RCT(88)*V(76)*V(83)
  A(89) = RCT(89)*V(83)*V(83)
  A(90) = RCT(90)*V(79)*V(84)
  A(91) = RCT(91)*V(22)
  A(92) = RCT(92)*V(78)*V(84)
  A(93) = RCT(93)*V(84)*V(85)
  A(94) = RCT(94)*V(80)*V(84)
  A(95) = RCT(95)*V(81)*V(84)
  A(96) = RCT(96)*V(77)*V(84)
  A(97) = RCT(97)*V(60)*V(84)
  A(98) = RCT(98)*V(84)*V(86)
  A(99) = RCT(99)*V(76)*V(84)
  A(100) = RCT(100)*V(83)*V(84)
  A(101) = RCT(101)*V(84)*V(84)
  A(102) = RCT(102)*V(75)*V(79)
  A(103) = RCT(103)*V(23)
  A(104) = RCT(104)*V(75)*V(78)
  A(105) = RCT(105)*V(75)*V(85)
  A(106) = RCT(106)*V(75)*V(80)
  A(107) = RCT(107)*V(75)*V(81)
  A(108) = RCT(108)*V(75)*V(77)
  A(109) = RCT(109)*V(60)*V(75)
  A(110) = RCT(110)*V(75)*V(86)
  A(111) = RCT(111)*V(75)*V(76)
  A(112) = RCT(112)*V(75)*V(83)
  A(113) = RCT(113)*V(75)*V(84)
  A(114) = RCT(114)*V(75)*V(75)
  A(115) = RCT(115)*V(31)*V(79)
  A(116) = RCT(116)*V(31)
  A(117) = RCT(117)*V(57)*V(79)
  A(118) = RCT(118)*V(57)*V(85)
  A(119) = RCT(119)*V(57)
  A(120) = RCT(120)*V(37)*V(79)
  A(121) = RCT(121)*V(37)*V(85)
  A(122) = RCT(122)*V(37)
  A(123) = RCT(123)*V(68)
  A(124) = RCT(124)*V(68)
  A(125) = RCT(125)*V(68)*V(82)
  A(126) = RCT(126)*V(68)*V(85)
  A(127) = RCT(127)*V(36)
  A(128) = RCT(128)*V(36)*V(78)
  A(129) = RCT(129)*V(68)*V(80)
  A(130) = RCT(130)*V(67)*V(82)
  A(131) = RCT(131)*V(67)
  A(132) = RCT(132)*V(67)*V(80)
  A(133) = RCT(133)*V(71)*V(82)
  A(134) = RCT(134)*V(71)
  A(135) = RCT(135)*V(71)*V(80)
  A(136) = RCT(136)*V(54)*V(82)
  A(137) = RCT(137)*V(54)
  A(138) = RCT(138)*V(72)*V(82)
  A(139) = RCT(139)*V(72)
  A(140) = RCT(140)*V(33)*V(82)
  A(141) = RCT(141)*V(25)*V(82)
  A(142) = RCT(142)*V(35)*V(82)
  A(143) = RCT(143)*V(35)
  A(144) = RCT(144)*V(48)*V(82)
  A(145) = RCT(145)*V(48)
  A(146) = RCT(146)*V(56)
  A(147) = RCT(147)*V(56)
  A(148) = RCT(148)*V(56)*V(82)
  A(149) = RCT(149)*V(56)*V(80)
  A(150) = RCT(150)*V(50)
  A(151) = RCT(151)*V(50)*V(82)
  A(152) = RCT(152)*V(50)*V(80)
  A(153) = RCT(153)*V(27)
  A(154) = RCT(154)*V(49)*V(82)
  A(155) = RCT(155)*V(49)*V(80)
  A(156) = RCT(156)*V(43)*V(82)
  A(157) = RCT(157)*V(43)*V(80)
  A(158) = RCT(158)*V(46)*V(80)
  A(159) = RCT(159)*V(47)*V(82)
  A(160) = RCT(160)*V(47)
  A(161) = RCT(161)*V(47)*V(80)
  A(162) = RCT(162)*V(62)*V(82)
  A(163) = RCT(163)*V(62)*V(74)
  A(164) = RCT(164)*V(62)*V(80)
  A(165) = RCT(165)*V(62)*V(70)
  A(166) = RCT(166)*V(62)
  A(167) = RCT(167)*V(66)*V(82)
  A(168) = RCT(168)*V(66)*V(74)
  A(169) = RCT(169)*V(66)*V(70)
  A(170) = RCT(170)*V(66)
  A(171) = RCT(171)*V(64)*V(82)
  A(172) = RCT(172)*V(64)*V(74)
  A(173) = RCT(173)*V(64)*V(80)
  A(174) = RCT(174)*V(64)
  A(175) = RCT(175)*V(73)*V(82)
  A(176) = RCT(176)*V(73)
  A(177) = RCT(177)*V(69)*V(82)
  A(178) = RCT(178)*V(69)
  A(179) = RCT(179)*V(45)*V(82)
  A(180) = RCT(180)*V(45)*V(74)
  A(181) = RCT(181)*V(42)*V(82)
  A(182) = RCT(182)*V(42)
  A(183) = RCT(183)*V(41)*V(82)
  A(184) = RCT(184)*V(41)
  A(185) = RCT(185)*V(15)*V(82)
  A(186) = RCT(186)*V(53)*V(82)
  A(187) = RCT(187)*V(53)*V(74)
  A(188) = RCT(188)*V(53)*V(80)
  A(189) = RCT(189)*V(53)*V(70)
  A(190) = RCT(190)*V(58)*V(82)
  A(191) = RCT(191)*V(18)*V(85)
  A(192) = RCT(192)*V(18)*V(78)
  A(193) = RCT(193)*V(18)*V(18)
  A(194) = RCT(194)*V(17)*V(82)
  A(195) = RCT(195)*V(7)*V(82)
  A(196) = RCT(196)*V(8)*V(85)
  A(197) = RCT(197)*V(8)*V(78)
  A(198) = RCT(198)*V(8)
  A(199) = RCT(199)*V(58)*V(74)
  A(200) = RCT(200)*V(58)*V(80)
  A(201) = RCT(201)*V(58)*V(70)
  A(202) = RCT(202)*V(59)*V(82)
  A(203) = RCT(203)*V(59)*V(74)
  A(204) = RCT(204)*V(59)*V(80)
  A(205) = RCT(205)*V(59)*V(70)
  A(206) = RCT(206)*V(19)*V(82)
  A(207) = RCT(207)*V(26)*V(82)
  A(208) = RCT(208)*V(44)*V(82)
  A(209) = RCT(209)*V(30)*V(82)
  A(210) = RCT(210)*V(39)*V(82)
  A(211) = RCT(211)*V(32)*V(82)
  A(212) = RCT(212)*V(40)*V(82)
  A(213) = RCT(213)*V(34)*V(82)
  A(214) = RCT(214)*V(63)*V(82)
  A(215) = RCT(215)*V(63)*V(74)
  A(216) = RCT(216)*V(63)*V(80)
  A(217) = RCT(217)*V(63)*V(70)
  A(218) = RCT(218)*V(65)*V(82)
  A(219) = RCT(219)*V(65)*V(74)
  A(220) = RCT(220)*V(65)*V(80)
  A(221) = RCT(221)*V(65)*V(70)
  A(222) = RCT(222)*V(44)*V(74)
  A(223) = RCT(223)*V(55)*V(82)
  A(224) = RCT(224)*V(55)*V(74)
  A(225) = RCT(225)*V(55)*V(80)
  A(226) = RCT(226)*V(55)*V(70)
  A(227) = RCT(227)*V(61)*V(82)
  A(228) = RCT(228)*V(61)*V(74)
  A(229) = RCT(229)*V(61)*V(80)
  A(230) = RCT(230)*V(61)*V(70)


  Vdot(1) = A(44)
  Vdot(2) = A(128)+0.333*A(163)+0.351*A(168)+0.1*A(172)+0.37*A(187)+0.204*A(199)+0.103*A(203)+0.297*A(208)+0.185*A(215)&
              &+0.073*A(219)+0.185*A(224)+0.103*A(228)
  Vdot(3) = 0.25*A(72)+A(74)+A(75)+A(77)+0.05*A(215)+0.129*A(219)+0.17*A(224)
  Vdot(4) = 0.25*A(82)+A(84)+A(85)+A(87)+0.25*A(93)+A(95)+A(96)+A(98)+0.25*A(105)+A(107)+A(108)+2*A(110)+0.372*A(172)&
              &+0.15*A(199)+0.189*A(203)+0.119*A(215)+0.247*A(219)+0.189*A(228)
  Vdot(5) = A(196)
  Vdot(6) = A(193)+A(195)+A(198)
  Vdot(7) = 0.75*A(194)-A(195)
  Vdot(8) = 0.12*A(194)-A(196)-A(197)-A(198)
  Vdot(9) = 0.5*A(222)+0.135*A(224)
  Vdot(10) = 0.75*A(72)
  Vdot(11) = 0.75*A(82)+0.75*A(93)+0.75*A(105)
  Vdot(12) = 2*A(120)+A(225)
  Vdot(13) = 6*A(120)+7*A(160)+0.048*A(223)+0.07*A(224)+2.693*A(225)+0.55*A(226)
  Vdot(14) = A(18)-A(19)-A(20)
  Vdot(15) = -A(185)
  Vdot(16) = -A(44)
  Vdot(17) = 0.88*A(191)-A(194)
  Vdot(18) = A(190)-A(191)-A(192)-2*A(193)+0.13*A(194)
  Vdot(19) = -A(206)
  Vdot(20) = A(69)-A(70)
  Vdot(21) = A(79)-A(80)
  Vdot(22) = A(90)-A(91)
  Vdot(23) = A(102)-A(103)
  Vdot(24) = A(37)+A(38)-A(41)-A(42)
  Vdot(25) = -A(141)
  Vdot(26) = -A(207)
  Vdot(27) = -A(153)+0.031*A(203)+0.087*A(213)+0.031*A(228)
  Vdot(28) = A(11)-A(12)-A(13)
  Vdot(29) = A(21)-A(22)-A(23)-A(24)
  Vdot(30) = -A(209)
  Vdot(31) = -A(115)-A(116)+0.236*A(209)
  Vdot(32) = -A(211)
  Vdot(33) = A(49)+0.25*A(54)+0.25*A(64)-A(140)
  Vdot(34) = -A(213)
  Vdot(35) = A(47)-A(142)-A(143)
  Vdot(36) = A(126)-A(127)-A(128)
  Vdot(37) = -A(120)-A(121)-A(122)+A(158)
  Vdot(38) = A(32)-A(33)-A(34)-A(35)
  Vdot(39) = -A(210)
  Vdot(40) = -A(212)
  Vdot(41) = -A(183)-A(184)+0.051*A(212)+0.093*A(213)
  Vdot(42) = -A(181)-A(182)+0.108*A(212)+0.099*A(213)
  Vdot(43) = -A(156)-A(157)+0.207*A(212)+0.187*A(213)
  Vdot(44) = -A(208)-A(222)
  Vdot(45) = -A(179)-A(180)+0.491*A(212)+0.561*A(213)
  Vdot(46) = A(117)+A(121)+A(122)-A(158)
  Vdot(47) = -A(159)-A(160)-A(161)+0.059*A(212)+0.05*A(213)+0.061*A(218)+0.042*A(219)+0.015*A(220)
  Vdot(48) = A(52)+A(63)-A(144)-A(145)
  Vdot(49) = A(118)+A(119)-A(154)-A(155)+0.017*A(212)
  Vdot(50) = -A(150)-A(151)-A(152)+0.23*A(156)+0.084*A(162)+0.9*A(163)+0.3*A(167)+0.95*A(168)+0.174*A(171)+0.742*A(172)&
               &+0.008*A(173)+0.5*A(182)+0.5*A(184)+0.119*A(212)+0.287*A(213)
  Vdot(51) = -A(29)+A(123)+A(124)+A(125)+A(129)+A(131)+0.034*A(133)+A(134)+2*A(146)+A(147)+1.26*A(148)+1.26*A(149)&
               &+A(150)+A(151)+A(152)+0.416*A(162)+0.45*A(163)+0.5*A(164)+0.67*A(166)+0.475*A(168)+0.7*A(170)+0.336*A(171)&
               &+0.498*A(172)+0.572*A(173)+1.233*A(174)+A(179)+1.5*A(180)+A(182)+A(184)+0.5*A(187)+0.491*A(189)+0.275*A(199)&
               &+0.157*A(203)+0.393*A(208)+0.002*A(210)+0.345*A(215)+0.265*A(219)+0.012*A(221)+1.5*A(222)+0.51*A(224)+0.157&
               &*A(228)
  Vdot(52) = 2*A(13)+A(25)-A(27)-A(28)+0.2*A(39)+A(129)+A(132)+A(135)+A(149)+A(152)+A(155)+A(157)+A(158)+A(161)+0.5&
               &*A(164)+0.15*A(173)
  Vdot(53) = -A(186)-A(187)-A(188)-A(189)
  Vdot(54) = A(116)-A(136)-A(137)+0.006*A(177)+0.02*A(178)+0.13*A(203)+0.704*A(207)+0.024*A(209)+0.452*A(210)+0.072&
               &*A(211)+0.005*A(214)+0.001*A(215)+0.024*A(216)+0.127*A(218)+0.045*A(219)+0.102*A(220)+0.13*A(228)
  Vdot(55) = -A(223)-A(224)-A(225)-A(226)
  Vdot(56) = -A(146)-A(147)-A(148)-A(149)+0.23*A(154)+0.15*A(171)+0.023*A(172)+A(180)+0.5*A(182)+0.5*A(184)+0.009*A(189)&
               &+0.001*A(203)+0.607*A(208)+0.118*A(212)+0.097*A(213)+0.001*A(228)
  Vdot(57) = A(92)+A(94)+A(99)+A(100)+2*A(101)+A(113)-A(117)-A(118)-A(119)+0.24*A(154)+A(155)+0.24*A(156)+A(157)
  Vdot(58) = -A(190)-A(199)-A(200)-A(201)
  Vdot(59) = -A(202)-A(203)-A(204)-A(205)
  Vdot(60) = -A(56)-A(57)-A(58)-A(59)-A(60)-A(67)-A(76)-A(86)+A(92)+A(94)-A(97)+A(99)+A(100)+2*A(101)-A(109)+A(113)&
               &+A(136)+0.616*A(138)+0.675*A(167)+0.515*A(176)+0.596*A(177)+0.152*A(178)+A(181)+A(182)+A(183)+A(184)+0.079&
               &*A(190)+0.126*A(199)+0.187*A(200)+0.24*A(201)+0.5*A(202)+0.729*A(203)+0.75*A(204)+0.559*A(209)+0.936*A(210)&
               &+0.948*A(211)+0.205*A(214)+0.488*A(216)+0.001*A(218)+0.137*A(219)+0.711*A(220)+0.5*A(227)+0.729*A(228)+0.75&
               &*A(229)
  Vdot(61) = -A(227)-A(228)-A(229)-A(230)
  Vdot(62) = -A(162)-A(163)-A(164)-A(165)-A(166)+0.23*A(190)+0.39*A(199)+0.025*A(218)+0.026*A(219)+0.012*A(221)
  Vdot(63) = -A(214)-A(215)-A(216)-A(217)
  Vdot(64) = -A(171)-A(172)-A(173)-A(174)+0.357*A(190)+0.936*A(200)+0.025*A(218)
  Vdot(65) = -A(218)-A(219)-A(220)-A(221)
  Vdot(66) = -A(167)-A(168)-A(169)-A(170)+0.32*A(190)+0.16*A(199)+0.019*A(219)+0.048*A(220)
  Vdot(67) = A(81)+A(83)+A(88)+2*A(89)+A(100)+A(112)-A(130)-A(131)-A(132)+0.034*A(133)+A(134)+0.482*A(138)+A(139)+0.96&
               &*A(141)+0.129*A(171)+0.047*A(172)+0.467*A(174)+0.084*A(175)+0.246*A(176)+0.439*A(177)+0.431*A(178)+0.195&
               &*A(186)+0.25*A(189)+A(206)+0.445*A(209)+0.455*A(210)+0.099*A(211)+0.294*A(214)+0.154*A(215)+0.009*A(216)&
               &+0.732*A(218)+0.456*A(219)+0.507*A(220)+0.984*A(223)+0.5*A(224)
  Vdot(68) = A(46)+A(48)+A(49)+2*A(50)+0.75*A(54)+0.75*A(64)+A(74)+A(84)+A(95)+A(104)+A(106)+A(107)+A(111)+A(112)+A(113)&
               &+2*A(114)-A(123)-A(124)-A(125)-A(126)+A(127)-A(129)+A(136)+0.115*A(138)+A(140)+0.081*A(141)+0.35*A(142)&
               &+A(143)+A(147)+0.084*A(162)+0.2*A(163)+0.67*A(166)+0.3*A(167)+0.1*A(168)+0.055*A(171)+0.125*A(172)+0.227&
               &*A(173)+0.3*A(174)+0.213*A(175)+0.506*A(176)+0.01*A(177)+0.134*A(178)+1.61*A(186)+A(187)+0.191*A(189)+0.624&
               &*A(190)+0.592*A(199)+0.24*A(201)+0.276*A(202)+0.235*A(203)+0.096*A(208)+0.026*A(209)+0.024*A(210)+0.026&
               &*A(211)+0.732*A(214)+0.5*A(215)+0.244*A(218)+0.269*A(219)+0.079*A(220)+0.984*A(223)+0.5*A(224)+0.276*A(227)&
               &+0.235*A(228)
  Vdot(69) = A(62)+A(115)+0.572*A(173)-0.69*A(177)-A(178)+0.276*A(204)+0.511*A(216)+0.321*A(220)+0.276*A(229)
  Vdot(70) = A(1)-A(2)-A(3)-A(4)-A(5)-A(6)+A(16)+A(17)+A(20)-A(165)-A(169)-A(189)-A(201)-A(205)-A(217)-A(221)-A(226)&
               &-A(230)
  Vdot(71) = -A(133)-A(134)-A(135)+0.37*A(138)+A(144)+A(145)+A(165)+0.675*A(167)+0.45*A(169)+0.013*A(171)+0.218*A(173)&
               &+0.558*A(175)+0.71*A(176)+0.213*A(177)+0.147*A(178)+A(179)+A(181)+A(183)+A(188)+0.474*A(202)+0.205*A(203)&
               &+0.474*A(204)+0.147*A(205)+0.261*A(207)+0.122*A(209)+0.244*A(210)+0.204*A(211)+0.497*A(214)+0.363*A(215)&
               &+0.037*A(216)+0.45*A(217)+0.511*A(218)+0.305*A(219)+0.151*A(220)+0.069*A(221)+0.45*A(226)+0.474*A(227)+0.205&
               &*A(228)+0.474*A(229)+0.147*A(230)
  Vdot(72) = 0.5*A(64)+A(65)+0.5*A(66)+A(68)-A(138)-A(139)+0.416*A(162)+0.55*A(169)+0.15*A(171)+0.21*A(172)+0.233*A(174)&
               &+0.115*A(175)+0.177*A(177)+0.243*A(178)+0.332*A(209)+0.11*A(210)+0.089*A(211)+0.437*A(217)+0.072*A(218)&
               &+0.026*A(219)+0.001*A(220)+0.659*A(221)+0.55*A(226)
  Vdot(73) = 0.5*A(64)+0.5*A(66)+A(68)+A(77)+A(87)+A(98)+0.7*A(170)+0.332*A(171)-0.671*A(175)-A(176)+0.048*A(177)+0.435&
               &*A(178)+0.1*A(199)+0.75*A(201)+0.276*A(202)+0.276*A(203)+0.853*A(205)+0.125*A(210)+0.417*A(211)+0.055*A(212)&
               &+0.119*A(214)+0.215*A(215)+0.113*A(217)+0.043*A(219)+0.259*A(221)+0.276*A(227)+0.276*A(228)+0.853*A(230)
  Vdot(74) = A(2)-A(3)-A(7)-A(8)-A(17)-A(18)-A(30)-A(36)+0.25*A(72)+0.25*A(82)+0.25*A(93)+0.25*A(105)-A(163)-A(168)&
               &-A(172)-A(180)-A(187)-A(199)-A(203)-A(215)-A(219)-A(222)-A(224)-A(228)
  Vdot(75) = -A(102)+A(103)-A(104)-A(105)-A(106)-A(107)-A(108)-A(110)-A(111)-A(112)-A(113)-2*A(114)+0.5*A(162)+0.5&
               &*A(164)+0.33*A(166)+0.3*A(170)+0.289*A(171)+0.15*A(173)+0.192*A(199)+0.24*A(201)
  Vdot(76) = -A(69)+A(70)-A(71)-A(72)-A(73)-A(74)-A(75)-A(77)-2*A(78)-A(88)-A(99)+A(104)+A(106)+A(112)+A(113)+2*A(114)&
               &+A(130)+A(132)+A(136)+A(137)+0.492*A(138)+A(139)+A(150)+A(151)+A(152)+2*A(153)+0.67*A(166)+0.675*A(167)&
               &+0.467*A(174)+0.029*A(175)+0.667*A(176)+A(181)+0.5*A(182)+A(183)+0.5*A(184)+0.123*A(203)+0.011*A(210)+0.137&
               &*A(219)+0.123*A(228)
  Vdot(77) = -A(51)-A(52)-A(53)-A(54)-2*A(55)-A(66)-A(75)+A(81)+A(83)-A(85)+A(88)+2*A(89)-A(96)+A(100)-A(108)+A(112)&
               &+0.034*A(133)+A(134)+0.37*A(138)+A(139)+0.05*A(141)+0.34*A(144)+0.76*A(154)+0.76*A(156)+0.5*A(162)+0.1&
               &*A(163)+0.5*A(164)+0.33*A(166)+0.3*A(167)+0.05*A(168)+0.67*A(171)+0.048*A(172)+0.799*A(173)+0.473*A(175)&
               &+0.96*A(176)+0.376*A(177)+0.564*A(178)+A(179)+A(182)+A(184)+A(186)+A(188)+0.2*A(189)+0.907*A(190)+0.066&
               &*A(199)+0.749*A(200)+0.75*A(202)+0.031*A(203)+0.276*A(204)+A(206)+0.965*A(207)+0.1*A(208)+0.695*A(209)+0.835&
               &*A(210)+0.653*A(211)+0.765*A(212)+0.804*A(213)+0.91*A(214)+0.022*A(215)+0.824*A(216)+0.918*A(218)+0.033&
               &*A(219)+0.442*A(220)+0.012*A(221)+0.984*A(223)+0.949*A(225)+0.75*A(227)+0.031*A(228)+0.276*A(229)
  Vdot(78) = A(1)-A(4)+A(5)-A(7)-A(9)-2*A(10)+A(14)+A(15)-A(21)+A(22)-A(31)-A(46)-A(51)-A(56)-A(62)-A(71)-A(81)-A(92)&
               &-A(104)-A(128)
  Vdot(79) = -A(1)+A(4)-A(5)-A(6)+A(7)-A(8)+2*A(9)+2*A(10)-A(11)+A(12)+A(16)+A(23)+A(24)-A(25)+A(26)+A(28)+A(31)-A(32)&
               &+A(33)+0.61*A(34)+A(35)+0.8*A(39)+2*A(40)+A(46)+A(48)+A(51)+A(53)+A(56)+A(58)+A(65)-A(69)+A(70)+A(71)+A(73)&
               &-A(79)+A(80)+A(81)+A(83)-A(90)+A(91)+A(92)+A(94)-A(102)+A(103)+A(104)+A(106)-A(115)-A(117)-A(120)+A(128)&
               &+0.338*A(177)+A(178)+0.187*A(200)+0.474*A(204)+0.391*A(220)+0.474*A(229)
  Vdot(80) = A(6)+A(8)-A(9)-A(11)+A(12)-A(14)-A(15)-A(16)-A(26)+A(27)+0.39*A(34)-A(39)-2*A(40)-A(48)-A(53)-A(58)-A(65)&
               &-A(73)-A(83)-A(94)-A(106)-A(129)-A(132)-A(135)-A(149)-A(152)-A(155)-A(157)-A(158)-A(161)-A(164)-A(173)&
               &-A(188)-A(200)-A(204)-A(216)-A(220)-A(225)-A(229)
  Vdot(81) = -A(46)-A(47)-A(48)-2*A(49)-2*A(50)-A(54)-A(64)+A(71)+A(73)-A(74)+2*A(78)-A(84)+A(88)-A(95)+A(99)-A(107)&
               &+A(111)+A(116)+A(131)+A(137)+0.65*A(142)+0.3*A(170)+A(185)+0.3*A(189)+0.25*A(201)+0.011*A(210)+0.076*A(215)&
               &+0.197*A(219)+0.03*A(220)+0.26*A(224)
  Vdot(82) = 2*A(19)-A(21)+A(22)-A(24)-A(25)-A(26)-A(27)+A(28)-A(29)-A(30)+A(31)+0.39*A(34)-A(35)+A(36)+0.8*A(39)+2&
               &*A(41)-A(42)-A(43)-A(44)-A(45)-A(125)-A(130)-A(133)-A(136)-A(138)-A(140)-A(141)-0.65*A(142)+A(143)-0.34&
               &*A(144)+A(145)-A(148)-A(151)-A(154)-A(156)-A(159)-A(162)+0.208*A(163)+0.33*A(166)-A(167)+0.164*A(168)-A(171)&
               &+0.285*A(172)-A(175)-A(177)-A(179)+0.5*A(180)-A(181)-A(183)-A(185)-A(186)+0.12*A(187)-A(190)+0.266*A(199)&
               &-A(202)+0.567*A(203)-A(206)-A(207)-0.397*A(208)-A(209)-A(210)-A(211)-A(212)-A(213)-A(214)+0.155*A(215)&
               &-A(218)+0.378*A(219)+0.5*A(222)-A(223)+0.32*A(224)-A(227)+0.567*A(228)
  Vdot(83) = -A(79)+A(80)-A(81)-A(82)-A(83)-A(84)-A(85)-A(87)-A(88)-2*A(89)-A(100)-A(112)+0.965*A(133)+A(135)+0.096&
               &*A(138)+0.37*A(148)+0.37*A(149)+0.1*A(163)+0.05*A(168)+0.048*A(172)+0.3*A(174)+0.049*A(175)+0.333*A(176)&
               &+0.201*A(203)+0.006*A(219)+0.201*A(228)
  Vdot(84) = -A(90)+A(91)-A(92)-A(93)-A(94)-A(95)-A(96)-A(98)-A(99)-A(100)-2*A(101)-A(113)+A(159)+A(161)
  Vdot(85) = A(23)+A(26)+A(29)+A(30)-A(31)-A(32)+A(33)+0.61*A(34)-A(36)-2*A(37)-2*A(38)-A(39)+A(42)-A(43)+A(44)+A(45)&
               &+A(46)-A(47)+A(48)+2*A(50)+A(51)-A(52)+A(53)+A(54)+A(55)-A(63)+A(64)+A(65)+A(66)+A(68)-A(72)-A(82)-A(93)&
               &-A(105)-A(118)-A(121)+2*A(123)+A(125)-A(126)+A(127)+A(128)+A(129)+A(131)+A(134)+A(140)+0.95*A(141)+A(143)&
               &+A(145)+2*A(146)+0.63*A(148)+0.63*A(149)+A(150)+0.008*A(163)+0.34*A(166)+0.064*A(168)+0.4*A(172)+1.233&
               &*A(174)+0.379*A(175)+0.113*A(177)+0.341*A(178)+1.5*A(180)+0.5*A(182)+0.5*A(184)+0.12*A(187)+0.5*A(189)+0.907&
               &*A(190)+0.033*A(203)+0.297*A(208)+0.224*A(212)+0.187*A(213)+0.056*A(215)+0.003*A(219)+0.013*A(221)+1.5&
               &*A(222)+0.06*A(224)+0.033*A(228)
  Vdot(86) = -A(62)-A(63)-A(64)-A(65)-A(66)-2*A(68)-A(77)-A(87)-A(98)-A(110)+0.001*A(133)+0.042*A(138)+0.025*A(167)&
               &+0.041*A(171)+0.051*A(173)+0.07*A(175)+0.04*A(176)+0.173*A(177)+0.095*A(178)+0.093*A(190)+0.008*A(199)+0.064&
               &*A(200)+0.01*A(201)+0.25*A(202)+0.18*A(203)+0.25*A(204)+0.035*A(207)+0.07*A(209)+0.143*A(210)+0.347*A(211)&
               &+0.011*A(212)+0.009*A(213)+0.09*A(214)+0.001*A(215)+0.176*A(216)+0.082*A(218)+0.002*A(219)+0.136*A(220)&
               &+0.001*A(221)+0.016*A(223)+0.051*A(225)+0.25*A(227)+0.18*A(228)+0.25*A(229)
      
END SUBROUTINE saprc99_Fun
















SUBROUTINE saprc99_IRRFun ( V, F, RCT, IRR )


  REAL(kind=dp) :: V(NVAR)

  REAL(kind=dp) :: F(NFIX)

  REAL(kind=dp) :: RCT(NREACT)

  REAL(kind=dp) :: IRR(NREACT)



  IRR(1) = RCT(1)*V(79)
  IRR(2) = RCT(2)*V(70)*F(2)
  IRR(3) = RCT(3)*V(70)*V(74)
  IRR(4) = RCT(4)*V(70)*V(78)*F(2)
  IRR(5) = RCT(5)*V(70)*V(79)
  IRR(6) = RCT(6)*V(70)*V(79)
  IRR(7) = RCT(7)*V(74)*V(78)
  IRR(8) = RCT(8)*V(74)*V(79)
  IRR(9) = RCT(9)*V(78)*V(80)
  IRR(10) = RCT(10)*V(78)*V(78)*F(2)
  IRR(11) = RCT(11)*V(79)*V(80)
  IRR(12) = RCT(12)*V(28)
  IRR(13) = RCT(13)*V(28)*F(1)
  IRR(14) = RCT(14)*V(79)*V(80)
  IRR(15) = RCT(15)*V(80)
  IRR(16) = RCT(16)*V(80)
  IRR(17) = RCT(17)*V(74)
  IRR(18) = RCT(18)*V(74)
  IRR(19) = RCT(19)*V(14)*F(1)
  IRR(20) = RCT(20)*V(14)*F(2)
  IRR(21) = RCT(21)*V(78)*V(82)
  IRR(22) = RCT(22)*V(29)
  IRR(23) = RCT(23)*V(29)
  IRR(24) = RCT(24)*V(29)*V(82)
  IRR(25) = RCT(25)*V(79)*V(82)
  IRR(26) = RCT(26)*V(80)*V(82)
  IRR(27) = RCT(27)*V(52)*V(82)
  IRR(28) = RCT(28)*V(52)
  IRR(29) = RCT(29)*V(51)*V(82)
  IRR(30) = RCT(30)*V(74)*V(82)
  IRR(31) = RCT(31)*V(78)*V(85)
  IRR(32) = RCT(32)*V(79)*V(85)
  IRR(33) = RCT(33)*V(38)
  IRR(34) = RCT(34)*V(38)
  IRR(35) = RCT(35)*V(38)*V(82)
  IRR(36) = RCT(36)*V(74)*V(85)
  IRR(37) = RCT(37)*V(85)*V(85)
  IRR(38) = RCT(38)*V(85)*V(85)*F(1)
  IRR(39) = RCT(39)*V(80)*V(85)
  IRR(40) = RCT(40)*V(80)*V(80)
  IRR(41) = RCT(41)*V(24)
  IRR(42) = RCT(42)*V(24)*V(82)
  IRR(43) = RCT(43)*V(82)*V(85)
  IRR(44) = RCT(44)*V(16)*V(82)
  IRR(45) = RCT(45)*V(82)*F(2)
  IRR(46) = RCT(46)*V(78)*V(81)
  IRR(47) = RCT(47)*V(81)*V(85)
  IRR(48) = RCT(48)*V(80)*V(81)
  IRR(49) = RCT(49)*V(81)*V(81)
  IRR(50) = RCT(50)*V(81)*V(81)
  IRR(51) = RCT(51)*V(77)*V(78)
  IRR(52) = RCT(52)*V(77)*V(85)
  IRR(53) = RCT(53)*V(77)*V(80)
  IRR(54) = RCT(54)*V(77)*V(81)
  IRR(55) = RCT(55)*V(77)*V(77)
  IRR(56) = RCT(56)*V(60)*V(78)
  IRR(57) = RCT(57)*V(60)*V(85)
  IRR(58) = RCT(58)*V(60)*V(80)
  IRR(59) = RCT(59)*V(60)*V(81)
  IRR(60) = RCT(60)*V(60)*V(77)
  IRR(62) = RCT(62)*V(78)*V(86)
  IRR(63) = RCT(63)*V(85)*V(86)
  IRR(64) = RCT(64)*V(81)*V(86)
  IRR(65) = RCT(65)*V(80)*V(86)
  IRR(66) = RCT(66)*V(77)*V(86)
  IRR(67) = RCT(67)*V(60)*V(86)
  IRR(68) = RCT(68)*V(86)*V(86)
  IRR(69) = RCT(69)*V(76)*V(79)
  IRR(70) = RCT(70)*V(20)
  IRR(71) = RCT(71)*V(76)*V(78)
  IRR(72) = RCT(72)*V(76)*V(85)
  IRR(73) = RCT(73)*V(76)*V(80)
  IRR(74) = RCT(74)*V(76)*V(81)
  IRR(75) = RCT(75)*V(76)*V(77)
  IRR(76) = RCT(76)*V(60)*V(76)
  IRR(77) = RCT(77)*V(76)*V(86)
  IRR(78) = RCT(78)*V(76)*V(76)
  IRR(79) = RCT(79)*V(79)*V(83)
  IRR(80) = RCT(80)*V(21)
  IRR(81) = RCT(81)*V(78)*V(83)
  IRR(82) = RCT(82)*V(83)*V(85)
  IRR(83) = RCT(83)*V(80)*V(83)
  IRR(84) = RCT(84)*V(81)*V(83)
  IRR(85) = RCT(85)*V(77)*V(83)
  IRR(86) = RCT(86)*V(60)*V(83)
  IRR(87) = RCT(87)*V(83)*V(86)
  IRR(88) = RCT(88)*V(76)*V(83)
  IRR(89) = RCT(89)*V(83)*V(83)
  IRR(90) = RCT(90)*V(79)*V(84)
  IRR(91) = RCT(91)*V(22)
  IRR(92) = RCT(92)*V(78)*V(84)
  IRR(93) = RCT(93)*V(84)*V(85)
  IRR(94) = RCT(94)*V(80)*V(84)
  IRR(95) = RCT(95)*V(81)*V(84)
  IRR(96) = RCT(96)*V(77)*V(84)
  IRR(97) = RCT(97)*V(60)*V(84)
  IRR(98) = RCT(98)*V(84)*V(86)
  IRR(99) = RCT(99)*V(76)*V(84)
  IRR(100) = RCT(100)*V(83)*V(84)
  IRR(101) = RCT(101)*V(84)*V(84)
  IRR(102) = RCT(102)*V(75)*V(79)
  IRR(103) = RCT(103)*V(23)
  IRR(104) = RCT(104)*V(75)*V(78)
  IRR(105) = RCT(105)*V(75)*V(85)
  IRR(106) = RCT(106)*V(75)*V(80)
  IRR(107) = RCT(107)*V(75)*V(81)
  IRR(108) = RCT(108)*V(75)*V(77)
  IRR(109) = RCT(109)*V(60)*V(75)
  IRR(110) = RCT(110)*V(75)*V(86)
  IRR(111) = RCT(111)*V(75)*V(76)
  IRR(112) = RCT(112)*V(75)*V(83)
  IRR(113) = RCT(113)*V(75)*V(84)
  IRR(114) = RCT(114)*V(75)*V(75)
  IRR(115) = RCT(115)*V(31)*V(79)
  IRR(116) = RCT(116)*V(31)
  IRR(117) = RCT(117)*V(57)*V(79)
  IRR(118) = RCT(118)*V(57)*V(85)
  IRR(119) = RCT(119)*V(57)
  IRR(120) = RCT(120)*V(37)*V(79)
  IRR(121) = RCT(121)*V(37)*V(85)
  IRR(122) = RCT(122)*V(37)
  IRR(123) = RCT(123)*V(68)
  IRR(124) = RCT(124)*V(68)
  IRR(125) = RCT(125)*V(68)*V(82)
  IRR(126) = RCT(126)*V(68)*V(85)
  IRR(127) = RCT(127)*V(36)
  IRR(128) = RCT(128)*V(36)*V(78)
  IRR(129) = RCT(129)*V(68)*V(80)
  IRR(130) = RCT(130)*V(67)*V(82)
  IRR(131) = RCT(131)*V(67)
  IRR(132) = RCT(132)*V(67)*V(80)
  IRR(133) = RCT(133)*V(71)*V(82)
  IRR(134) = RCT(134)*V(71)
  IRR(135) = RCT(135)*V(71)*V(80)
  IRR(136) = RCT(136)*V(54)*V(82)
  IRR(137) = RCT(137)*V(54)
  IRR(138) = RCT(138)*V(72)*V(82)
  IRR(139) = RCT(139)*V(72)
  IRR(140) = RCT(140)*V(33)*V(82)
  IRR(141) = RCT(141)*V(25)*V(82)
  IRR(142) = RCT(142)*V(35)*V(82)
  IRR(143) = RCT(143)*V(35)
  IRR(144) = RCT(144)*V(48)*V(82)
  IRR(145) = RCT(145)*V(48)
  IRR(146) = RCT(146)*V(56)
  IRR(147) = RCT(147)*V(56)
  IRR(148) = RCT(148)*V(56)*V(82)
  IRR(149) = RCT(149)*V(56)*V(80)
  IRR(150) = RCT(150)*V(50)
  IRR(151) = RCT(151)*V(50)*V(82)
  IRR(152) = RCT(152)*V(50)*V(80)
  IRR(153) = RCT(153)*V(27)
  IRR(154) = RCT(154)*V(49)*V(82)
  IRR(155) = RCT(155)*V(49)*V(80)
  IRR(156) = RCT(156)*V(43)*V(82)
  IRR(157) = RCT(157)*V(43)*V(80)
  IRR(158) = RCT(158)*V(46)*V(80)
  IRR(159) = RCT(159)*V(47)*V(82)
  IRR(160) = RCT(160)*V(47)
  IRR(161) = RCT(161)*V(47)*V(80)
  IRR(162) = RCT(162)*V(62)*V(82)
  IRR(163) = RCT(163)*V(62)*V(74)
  IRR(164) = RCT(164)*V(62)*V(80)
  IRR(165) = RCT(165)*V(62)*V(70)
  IRR(166) = RCT(166)*V(62)
  IRR(167) = RCT(167)*V(66)*V(82)
  IRR(168) = RCT(168)*V(66)*V(74)
  IRR(169) = RCT(169)*V(66)*V(70)
  IRR(170) = RCT(170)*V(66)
  IRR(171) = RCT(171)*V(64)*V(82)
  IRR(172) = RCT(172)*V(64)*V(74)
  IRR(173) = RCT(173)*V(64)*V(80)
  IRR(174) = RCT(174)*V(64)
  IRR(175) = RCT(175)*V(73)*V(82)
  IRR(176) = RCT(176)*V(73)
  IRR(177) = RCT(177)*V(69)*V(82)
  IRR(178) = RCT(178)*V(69)
  IRR(179) = RCT(179)*V(45)*V(82)
  IRR(180) = RCT(180)*V(45)*V(74)
  IRR(181) = RCT(181)*V(42)*V(82)
  IRR(182) = RCT(182)*V(42)
  IRR(183) = RCT(183)*V(41)*V(82)
  IRR(184) = RCT(184)*V(41)
  IRR(185) = RCT(185)*V(15)*V(82)
  IRR(186) = RCT(186)*V(53)*V(82)
  IRR(187) = RCT(187)*V(53)*V(74)
  IRR(188) = RCT(188)*V(53)*V(80)
  IRR(189) = RCT(189)*V(53)*V(70)
  IRR(190) = RCT(190)*V(58)*V(82)
  IRR(191) = RCT(191)*V(18)*V(85)
  IRR(192) = RCT(192)*V(18)*V(78)
  IRR(193) = RCT(193)*V(18)*V(18)
  IRR(194) = RCT(194)*V(17)*V(82)
  IRR(195) = RCT(195)*V(7)*V(82)
  IRR(196) = RCT(196)*V(8)*V(85)
  IRR(197) = RCT(197)*V(8)*V(78)
  IRR(198) = RCT(198)*V(8)
  IRR(199) = RCT(199)*V(58)*V(74)
  IRR(200) = RCT(200)*V(58)*V(80)
  IRR(201) = RCT(201)*V(58)*V(70)
  IRR(202) = RCT(202)*V(59)*V(82)
  IRR(203) = RCT(203)*V(59)*V(74)
  IRR(204) = RCT(204)*V(59)*V(80)
  IRR(205) = RCT(205)*V(59)*V(70)
  IRR(206) = RCT(206)*V(19)*V(82)
  IRR(207) = RCT(207)*V(26)*V(82)
  IRR(208) = RCT(208)*V(44)*V(82)
  IRR(209) = RCT(209)*V(30)*V(82)
  IRR(210) = RCT(210)*V(39)*V(82)
  IRR(211) = RCT(211)*V(32)*V(82)
  IRR(212) = RCT(212)*V(40)*V(82)
  IRR(213) = RCT(213)*V(34)*V(82)
  IRR(214) = RCT(214)*V(63)*V(82)
  IRR(215) = RCT(215)*V(63)*V(74)
  IRR(216) = RCT(216)*V(63)*V(80)
  IRR(217) = RCT(217)*V(63)*V(70)
  IRR(218) = RCT(218)*V(65)*V(82)
  IRR(219) = RCT(219)*V(65)*V(74)
  IRR(220) = RCT(220)*V(65)*V(80)
  IRR(221) = RCT(221)*V(65)*V(70)
  IRR(222) = RCT(222)*V(44)*V(74)
  IRR(223) = RCT(223)*V(55)*V(82)
  IRR(224) = RCT(224)*V(55)*V(74)
  IRR(225) = RCT(225)*V(55)*V(80)
  IRR(226) = RCT(226)*V(55)*V(70)
  IRR(227) = RCT(227)*V(61)*V(82)
  IRR(228) = RCT(228)*V(61)*V(74)
  IRR(229) = RCT(229)*V(61)*V(80)
  IRR(230) = RCT(230)*V(61)*V(70)
      
END SUBROUTINE saprc99_IRRFun
















SUBROUTINE saprc99_Jac_SP ( V, F, RCT, JVS )


  REAL(kind=dp) :: V(NVAR)

  REAL(kind=dp) :: F(NFIX)

  REAL(kind=dp) :: RCT(NREACT)

  REAL(kind=dp) :: JVS(LU_NONZERO)




  REAL(kind=dp) :: B(408)


  B(1) = RCT(1)

  B(2) = RCT(2)*F(2)

  B(4) = RCT(3)*V(74)

  B(5) = RCT(3)*V(70)

  B(6) = RCT(4)*V(78)*F(2)

  B(7) = RCT(4)*V(70)*F(2)

  B(9) = RCT(5)*V(79)

  B(10) = RCT(5)*V(70)

  B(11) = RCT(6)*V(79)

  B(12) = RCT(6)*V(70)

  B(13) = RCT(7)*V(78)

  B(14) = RCT(7)*V(74)

  B(15) = RCT(8)*V(79)

  B(16) = RCT(8)*V(74)

  B(17) = RCT(9)*V(80)

  B(18) = RCT(9)*V(78)

  B(19) = RCT(10)*2*V(78)*F(2)

  B(21) = RCT(11)*V(80)

  B(22) = RCT(11)*V(79)

  B(23) = RCT(12)

  B(24) = RCT(13)*F(1)

  B(26) = RCT(14)*V(80)

  B(27) = RCT(14)*V(79)

  B(28) = RCT(15)

  B(29) = RCT(16)

  B(30) = RCT(17)

  B(31) = RCT(18)

  B(32) = RCT(19)*F(1)

  B(34) = RCT(20)*F(2)

  B(36) = RCT(21)*V(82)

  B(37) = RCT(21)*V(78)

  B(38) = RCT(22)

  B(39) = RCT(23)

  B(40) = RCT(24)*V(82)

  B(41) = RCT(24)*V(29)

  B(42) = RCT(25)*V(82)

  B(43) = RCT(25)*V(79)

  B(44) = RCT(26)*V(82)

  B(45) = RCT(26)*V(80)

  B(46) = RCT(27)*V(82)

  B(47) = RCT(27)*V(52)

  B(48) = RCT(28)

  B(49) = RCT(29)*V(82)

  B(50) = RCT(29)*V(51)

  B(51) = RCT(30)*V(82)

  B(52) = RCT(30)*V(74)

  B(53) = RCT(31)*V(85)

  B(54) = RCT(31)*V(78)

  B(55) = RCT(32)*V(85)

  B(56) = RCT(32)*V(79)

  B(57) = RCT(33)

  B(58) = RCT(34)

  B(59) = RCT(35)*V(82)

  B(60) = RCT(35)*V(38)

  B(61) = RCT(36)*V(85)

  B(62) = RCT(36)*V(74)

  B(63) = RCT(37)*2*V(85)

  B(64) = RCT(38)*2*V(85)*F(1)

  B(66) = RCT(39)*V(85)

  B(67) = RCT(39)*V(80)

  B(68) = RCT(40)*2*V(80)

  B(69) = RCT(41)

  B(70) = RCT(42)*V(82)

  B(71) = RCT(42)*V(24)

  B(72) = RCT(43)*V(85)

  B(73) = RCT(43)*V(82)

  B(74) = RCT(44)*V(82)

  B(75) = RCT(44)*V(16)

  B(76) = RCT(45)*F(2)

  B(78) = RCT(46)*V(81)

  B(79) = RCT(46)*V(78)

  B(80) = RCT(47)*V(85)

  B(81) = RCT(47)*V(81)

  B(82) = RCT(48)*V(81)

  B(83) = RCT(48)*V(80)

  B(84) = RCT(49)*2*V(81)

  B(85) = RCT(50)*2*V(81)

  B(86) = RCT(51)*V(78)

  B(87) = RCT(51)*V(77)

  B(88) = RCT(52)*V(85)

  B(89) = RCT(52)*V(77)

  B(90) = RCT(53)*V(80)

  B(91) = RCT(53)*V(77)

  B(92) = RCT(54)*V(81)

  B(93) = RCT(54)*V(77)

  B(94) = RCT(55)*2*V(77)

  B(95) = RCT(56)*V(78)

  B(96) = RCT(56)*V(60)

  B(97) = RCT(57)*V(85)

  B(98) = RCT(57)*V(60)

  B(99) = RCT(58)*V(80)

  B(100) = RCT(58)*V(60)

  B(101) = RCT(59)*V(81)

  B(102) = RCT(59)*V(60)

  B(103) = RCT(60)*V(77)

  B(104) = RCT(60)*V(60)

  B(105) = RCT(61)*2*V(60)

  B(106) = RCT(62)*V(86)

  B(107) = RCT(62)*V(78)

  B(108) = RCT(63)*V(86)

  B(109) = RCT(63)*V(85)

  B(110) = RCT(64)*V(86)

  B(111) = RCT(64)*V(81)

  B(112) = RCT(65)*V(86)

  B(113) = RCT(65)*V(80)

  B(114) = RCT(66)*V(86)

  B(115) = RCT(66)*V(77)

  B(116) = RCT(67)*V(86)

  B(117) = RCT(67)*V(60)

  B(118) = RCT(68)*2*V(86)

  B(119) = RCT(69)*V(79)

  B(120) = RCT(69)*V(76)

  B(121) = RCT(70)

  B(122) = RCT(71)*V(78)

  B(123) = RCT(71)*V(76)

  B(124) = RCT(72)*V(85)

  B(125) = RCT(72)*V(76)

  B(126) = RCT(73)*V(80)

  B(127) = RCT(73)*V(76)

  B(128) = RCT(74)*V(81)

  B(129) = RCT(74)*V(76)

  B(130) = RCT(75)*V(77)

  B(131) = RCT(75)*V(76)

  B(132) = RCT(76)*V(76)

  B(133) = RCT(76)*V(60)

  B(134) = RCT(77)*V(86)

  B(135) = RCT(77)*V(76)

  B(136) = RCT(78)*2*V(76)

  B(137) = RCT(79)*V(83)

  B(138) = RCT(79)*V(79)

  B(139) = RCT(80)

  B(140) = RCT(81)*V(83)

  B(141) = RCT(81)*V(78)

  B(142) = RCT(82)*V(85)

  B(143) = RCT(82)*V(83)

  B(144) = RCT(83)*V(83)

  B(145) = RCT(83)*V(80)

  B(146) = RCT(84)*V(83)

  B(147) = RCT(84)*V(81)

  B(148) = RCT(85)*V(83)

  B(149) = RCT(85)*V(77)

  B(150) = RCT(86)*V(83)

  B(151) = RCT(86)*V(60)

  B(152) = RCT(87)*V(86)

  B(153) = RCT(87)*V(83)

  B(154) = RCT(88)*V(83)

  B(155) = RCT(88)*V(76)

  B(156) = RCT(89)*2*V(83)

  B(157) = RCT(90)*V(84)

  B(158) = RCT(90)*V(79)

  B(159) = RCT(91)

  B(160) = RCT(92)*V(84)

  B(161) = RCT(92)*V(78)

  B(162) = RCT(93)*V(85)

  B(163) = RCT(93)*V(84)

  B(164) = RCT(94)*V(84)

  B(165) = RCT(94)*V(80)

  B(166) = RCT(95)*V(84)

  B(167) = RCT(95)*V(81)

  B(168) = RCT(96)*V(84)

  B(169) = RCT(96)*V(77)

  B(170) = RCT(97)*V(84)

  B(171) = RCT(97)*V(60)

  B(172) = RCT(98)*V(86)

  B(173) = RCT(98)*V(84)

  B(174) = RCT(99)*V(84)

  B(175) = RCT(99)*V(76)

  B(176) = RCT(100)*V(84)

  B(177) = RCT(100)*V(83)

  B(178) = RCT(101)*2*V(84)

  B(179) = RCT(102)*V(79)

  B(180) = RCT(102)*V(75)

  B(181) = RCT(103)

  B(182) = RCT(104)*V(78)

  B(183) = RCT(104)*V(75)

  B(184) = RCT(105)*V(85)

  B(185) = RCT(105)*V(75)

  B(186) = RCT(106)*V(80)

  B(187) = RCT(106)*V(75)

  B(188) = RCT(107)*V(81)

  B(189) = RCT(107)*V(75)

  B(190) = RCT(108)*V(77)

  B(191) = RCT(108)*V(75)

  B(192) = RCT(109)*V(75)

  B(193) = RCT(109)*V(60)

  B(194) = RCT(110)*V(86)

  B(195) = RCT(110)*V(75)

  B(196) = RCT(111)*V(76)

  B(197) = RCT(111)*V(75)

  B(198) = RCT(112)*V(83)

  B(199) = RCT(112)*V(75)

  B(200) = RCT(113)*V(84)

  B(201) = RCT(113)*V(75)

  B(202) = RCT(114)*2*V(75)

  B(203) = RCT(115)*V(79)

  B(204) = RCT(115)*V(31)

  B(205) = RCT(116)

  B(206) = RCT(117)*V(79)

  B(207) = RCT(117)*V(57)

  B(208) = RCT(118)*V(85)

  B(209) = RCT(118)*V(57)

  B(210) = RCT(119)

  B(211) = RCT(120)*V(79)

  B(212) = RCT(120)*V(37)

  B(213) = RCT(121)*V(85)

  B(214) = RCT(121)*V(37)

  B(215) = RCT(122)

  B(216) = RCT(123)

  B(217) = RCT(124)

  B(218) = RCT(125)*V(82)

  B(219) = RCT(125)*V(68)

  B(220) = RCT(126)*V(85)

  B(221) = RCT(126)*V(68)

  B(222) = RCT(127)

  B(223) = RCT(128)*V(78)

  B(224) = RCT(128)*V(36)

  B(225) = RCT(129)*V(80)

  B(226) = RCT(129)*V(68)

  B(227) = RCT(130)*V(82)

  B(228) = RCT(130)*V(67)

  B(229) = RCT(131)

  B(230) = RCT(132)*V(80)

  B(231) = RCT(132)*V(67)

  B(232) = RCT(133)*V(82)

  B(233) = RCT(133)*V(71)

  B(234) = RCT(134)

  B(235) = RCT(135)*V(80)

  B(236) = RCT(135)*V(71)

  B(237) = RCT(136)*V(82)

  B(238) = RCT(136)*V(54)

  B(239) = RCT(137)

  B(240) = RCT(138)*V(82)

  B(241) = RCT(138)*V(72)

  B(242) = RCT(139)

  B(243) = RCT(140)*V(82)

  B(244) = RCT(140)*V(33)

  B(245) = RCT(141)*V(82)

  B(246) = RCT(141)*V(25)

  B(247) = RCT(142)*V(82)

  B(248) = RCT(142)*V(35)

  B(249) = RCT(143)

  B(250) = RCT(144)*V(82)

  B(251) = RCT(144)*V(48)

  B(252) = RCT(145)

  B(253) = RCT(146)

  B(254) = RCT(147)

  B(255) = RCT(148)*V(82)

  B(256) = RCT(148)*V(56)

  B(257) = RCT(149)*V(80)

  B(258) = RCT(149)*V(56)

  B(259) = RCT(150)

  B(260) = RCT(151)*V(82)

  B(261) = RCT(151)*V(50)

  B(262) = RCT(152)*V(80)

  B(263) = RCT(152)*V(50)

  B(264) = RCT(153)

  B(265) = RCT(154)*V(82)

  B(266) = RCT(154)*V(49)

  B(267) = RCT(155)*V(80)

  B(268) = RCT(155)*V(49)

  B(269) = RCT(156)*V(82)

  B(270) = RCT(156)*V(43)

  B(271) = RCT(157)*V(80)

  B(272) = RCT(157)*V(43)

  B(273) = RCT(158)*V(80)

  B(274) = RCT(158)*V(46)

  B(275) = RCT(159)*V(82)

  B(276) = RCT(159)*V(47)

  B(277) = RCT(160)

  B(278) = RCT(161)*V(80)

  B(279) = RCT(161)*V(47)

  B(280) = RCT(162)*V(82)

  B(281) = RCT(162)*V(62)

  B(282) = RCT(163)*V(74)

  B(283) = RCT(163)*V(62)

  B(284) = RCT(164)*V(80)

  B(285) = RCT(164)*V(62)

  B(286) = RCT(165)*V(70)

  B(287) = RCT(165)*V(62)

  B(288) = RCT(166)

  B(289) = RCT(167)*V(82)

  B(290) = RCT(167)*V(66)

  B(291) = RCT(168)*V(74)

  B(292) = RCT(168)*V(66)

  B(293) = RCT(169)*V(70)

  B(294) = RCT(169)*V(66)

  B(295) = RCT(170)

  B(296) = RCT(171)*V(82)

  B(297) = RCT(171)*V(64)

  B(298) = RCT(172)*V(74)

  B(299) = RCT(172)*V(64)

  B(300) = RCT(173)*V(80)

  B(301) = RCT(173)*V(64)

  B(302) = RCT(174)

  B(303) = RCT(175)*V(82)

  B(304) = RCT(175)*V(73)

  B(305) = RCT(176)

  B(306) = RCT(177)*V(82)

  B(307) = RCT(177)*V(69)

  B(308) = RCT(178)

  B(309) = RCT(179)*V(82)

  B(310) = RCT(179)*V(45)

  B(311) = RCT(180)*V(74)

  B(312) = RCT(180)*V(45)

  B(313) = RCT(181)*V(82)

  B(314) = RCT(181)*V(42)

  B(315) = RCT(182)

  B(316) = RCT(183)*V(82)

  B(317) = RCT(183)*V(41)

  B(318) = RCT(184)

  B(319) = RCT(185)*V(82)

  B(320) = RCT(185)*V(15)

  B(321) = RCT(186)*V(82)

  B(322) = RCT(186)*V(53)

  B(323) = RCT(187)*V(74)

  B(324) = RCT(187)*V(53)

  B(325) = RCT(188)*V(80)

  B(326) = RCT(188)*V(53)

  B(327) = RCT(189)*V(70)

  B(328) = RCT(189)*V(53)

  B(329) = RCT(190)*V(82)

  B(330) = RCT(190)*V(58)

  B(331) = RCT(191)*V(85)

  B(332) = RCT(191)*V(18)

  B(333) = RCT(192)*V(78)

  B(334) = RCT(192)*V(18)

  B(335) = RCT(193)*2*V(18)

  B(336) = RCT(194)*V(82)

  B(337) = RCT(194)*V(17)

  B(338) = RCT(195)*V(82)

  B(339) = RCT(195)*V(7)

  B(340) = RCT(196)*V(85)

  B(341) = RCT(196)*V(8)

  B(342) = RCT(197)*V(78)

  B(343) = RCT(197)*V(8)

  B(344) = RCT(198)

  B(345) = RCT(199)*V(74)

  B(346) = RCT(199)*V(58)

  B(347) = RCT(200)*V(80)

  B(348) = RCT(200)*V(58)

  B(349) = RCT(201)*V(70)

  B(350) = RCT(201)*V(58)

  B(351) = RCT(202)*V(82)

  B(352) = RCT(202)*V(59)

  B(353) = RCT(203)*V(74)

  B(354) = RCT(203)*V(59)

  B(355) = RCT(204)*V(80)

  B(356) = RCT(204)*V(59)

  B(357) = RCT(205)*V(70)

  B(358) = RCT(205)*V(59)

  B(359) = RCT(206)*V(82)

  B(360) = RCT(206)*V(19)

  B(361) = RCT(207)*V(82)

  B(362) = RCT(207)*V(26)

  B(363) = RCT(208)*V(82)

  B(364) = RCT(208)*V(44)

  B(365) = RCT(209)*V(82)

  B(366) = RCT(209)*V(30)

  B(367) = RCT(210)*V(82)

  B(368) = RCT(210)*V(39)

  B(369) = RCT(211)*V(82)

  B(370) = RCT(211)*V(32)

  B(371) = RCT(212)*V(82)

  B(372) = RCT(212)*V(40)

  B(373) = RCT(213)*V(82)

  B(374) = RCT(213)*V(34)

  B(375) = RCT(214)*V(82)

  B(376) = RCT(214)*V(63)

  B(377) = RCT(215)*V(74)

  B(378) = RCT(215)*V(63)

  B(379) = RCT(216)*V(80)

  B(380) = RCT(216)*V(63)

  B(381) = RCT(217)*V(70)

  B(382) = RCT(217)*V(63)

  B(383) = RCT(218)*V(82)

  B(384) = RCT(218)*V(65)

  B(385) = RCT(219)*V(74)

  B(386) = RCT(219)*V(65)

  B(387) = RCT(220)*V(80)

  B(388) = RCT(220)*V(65)

  B(389) = RCT(221)*V(70)

  B(390) = RCT(221)*V(65)

  B(391) = RCT(222)*V(74)

  B(392) = RCT(222)*V(44)

  B(393) = RCT(223)*V(82)

  B(394) = RCT(223)*V(55)

  B(395) = RCT(224)*V(74)

  B(396) = RCT(224)*V(55)

  B(397) = RCT(225)*V(80)

  B(398) = RCT(225)*V(55)

  B(399) = RCT(226)*V(70)

  B(400) = RCT(226)*V(55)

  B(401) = RCT(227)*V(82)

  B(402) = RCT(227)*V(61)

  B(403) = RCT(228)*V(74)

  B(404) = RCT(228)*V(61)

  B(405) = RCT(229)*V(80)

  B(406) = RCT(229)*V(61)

  B(407) = RCT(230)*V(70)

  B(408) = RCT(230)*V(61)



  JVS(1) = 0

  JVS(2) = B(74)

  JVS(3) = B(75)

  JVS(4) = 0

  JVS(5) = B(223)

  JVS(6) = 0.297*B(363)

  JVS(7) = 0.37*B(323)

  JVS(8) = 0.185*B(395)

  JVS(9) = 0.204*B(345)

  JVS(10) = 0.103*B(353)

  JVS(11) = 0.103*B(403)

  JVS(12) = 0.333*B(282)

  JVS(13) = 0.185*B(377)

  JVS(14) = 0.1*B(298)

  JVS(15) = 0.073*B(385)

  JVS(16) = 0.351*B(291)

  JVS(17) = 0.333*B(283)+0.351*B(292)+0.1*B(299)+0.37*B(324)+0.204*B(346)+0.103*B(354)+0.185*B(378)+0.073*B(386)+0.185&
              &*B(396)+0.103*B(404)

  JVS(18) = B(224)

  JVS(19) = 0.297*B(364)

  JVS(20) = 0

  JVS(21) = 0.17*B(395)

  JVS(22) = 0.05*B(377)

  JVS(23) = 0.129*B(385)

  JVS(24) = 0.05*B(378)+0.129*B(386)+0.17*B(396)

  JVS(25) = 0.25*B(124)+B(128)+B(130)+B(134)

  JVS(26) = B(131)

  JVS(27) = B(129)

  JVS(28) = 0.25*B(125)

  JVS(29) = B(135)

  JVS(30) = 0

  JVS(31) = 0.15*B(345)

  JVS(32) = 0.189*B(353)

  JVS(33) = 0.189*B(403)

  JVS(34) = 0.119*B(377)

  JVS(35) = 0.372*B(298)

  JVS(36) = 0.247*B(385)

  JVS(37) = 0.372*B(299)+0.15*B(346)+0.189*B(354)+0.119*B(378)+0.247*B(386)+0.189*B(404)

  JVS(38) = 0.25*B(184)+B(188)+B(190)+2*B(194)

  JVS(39) = B(148)+B(168)+B(191)

  JVS(40) = B(146)+B(166)+B(189)

  JVS(41) = 0.25*B(142)+B(147)+B(149)+B(152)

  JVS(42) = 0.25*B(162)+B(167)+B(169)+B(172)

  JVS(43) = 0.25*B(143)+0.25*B(163)+0.25*B(185)

  JVS(44) = B(153)+B(173)+2*B(195)

  JVS(45) = 0

  JVS(46) = B(340)

  JVS(47) = B(341)

  JVS(48) = 0

  JVS(49) = B(338)

  JVS(50) = B(344)

  JVS(51) = B(335)

  JVS(52) = B(339)

  JVS(53) = -B(338)

  JVS(54) = 0.75*B(336)

  JVS(55) = 0.75*B(337)-B(339)

  JVS(56) = -B(340)-B(342)-B(344)

  JVS(57) = 0.12*B(336)

  JVS(58) = -B(343)

  JVS(59) = 0.12*B(337)

  JVS(60) = -B(341)

  JVS(61) = 0

  JVS(62) = 0.5*B(391)

  JVS(63) = 0.135*B(395)

  JVS(64) = 0.5*B(392)+0.135*B(396)

  JVS(65) = 0

  JVS(66) = 0.75*B(124)

  JVS(67) = 0.75*B(125)

  JVS(68) = 0

  JVS(69) = 0.75*B(184)

  JVS(70) = 0.75*B(142)

  JVS(71) = 0.75*B(162)

  JVS(72) = 0.75*B(143)+0.75*B(163)+0.75*B(185)

  JVS(73) = 0

  JVS(74) = 2*B(211)

  JVS(75) = B(397)

  JVS(76) = 2*B(212)

  JVS(77) = B(398)

  JVS(78) = 0

  JVS(79) = 6*B(211)

  JVS(80) = 7*B(277)

  JVS(81) = 0.048*B(393)+0.07*B(395)+2.693*B(397)+0.55*B(399)

  JVS(82) = 0.55*B(400)

  JVS(83) = 0.07*B(396)

  JVS(84) = 6*B(212)

  JVS(85) = 2.693*B(398)

  JVS(86) = 0.048*B(394)

  JVS(87) = -B(32)-B(34)

  JVS(88) = B(31)

  JVS(89) = -B(319)

  JVS(90) = -B(320)

  JVS(91) = -B(74)

  JVS(92) = -B(75)

  JVS(93) = -B(336)

  JVS(94) = 0.88*B(331)

  JVS(95) = -B(337)

  JVS(96) = 0.88*B(332)

  JVS(97) = 0.13*B(336)

  JVS(98) = -B(331)-B(333)-2*B(335)

  JVS(99) = B(329)

  JVS(100) = -B(334)

  JVS(101) = B(330)+0.13*B(337)

  JVS(102) = -B(332)

  JVS(103) = -B(359)

  JVS(104) = -B(360)

  JVS(105) = -B(121)

  JVS(106) = B(119)

  JVS(107) = B(120)

  JVS(108) = -B(139)

  JVS(109) = B(137)

  JVS(110) = B(138)

  JVS(111) = -B(159)

  JVS(112) = B(157)

  JVS(113) = B(158)

  JVS(114) = -B(181)

  JVS(115) = B(179)

  JVS(116) = B(180)

  JVS(117) = -B(69)-B(70)

  JVS(118) = -B(71)

  JVS(119) = B(63)+B(64)

  JVS(120) = -B(245)

  JVS(121) = -B(246)

  JVS(122) = -B(361)

  JVS(123) = -B(362)

  JVS(124) = -B(264)

  JVS(125) = 0.087*B(373)

  JVS(126) = 0.031*B(353)

  JVS(127) = 0.031*B(403)

  JVS(128) = 0.031*B(354)+0.031*B(404)

  JVS(129) = 0.087*B(374)

  JVS(130) = -B(23)-B(24)

  JVS(131) = B(21)

  JVS(132) = B(22)

  JVS(133) = -B(38)-B(39)-B(40)

  JVS(134) = B(36)

  JVS(135) = B(37)-B(41)

  JVS(136) = -B(365)

  JVS(137) = -B(366)

  JVS(138) = 0.236*B(365)

  JVS(139) = -B(203)-B(205)

  JVS(140) = -B(204)

  JVS(141) = 0.236*B(366)

  JVS(142) = -B(369)

  JVS(143) = -B(370)

  JVS(144) = -B(243)

  JVS(145) = 0.25*B(92)

  JVS(146) = B(84)+0.25*B(93)+0.25*B(110)

  JVS(147) = -B(244)

  JVS(148) = 0.25*B(111)

  JVS(149) = -B(373)

  JVS(150) = -B(374)

  JVS(151) = -B(247)-B(249)

  JVS(152) = B(80)

  JVS(153) = -B(248)

  JVS(154) = B(81)

  JVS(155) = -B(222)-B(223)

  JVS(156) = B(220)

  JVS(157) = -B(224)

  JVS(158) = B(221)

  JVS(159) = -B(211)-B(213)-B(215)

  JVS(160) = B(273)

  JVS(161) = -B(212)

  JVS(162) = B(274)

  JVS(163) = -B(214)

  JVS(164) = -B(57)-B(58)-B(59)

  JVS(165) = B(55)

  JVS(166) = -B(60)

  JVS(167) = B(56)

  JVS(168) = -B(367)

  JVS(169) = -B(368)

  JVS(170) = -B(371)

  JVS(171) = -B(372)

  JVS(172) = 0.093*B(373)

  JVS(173) = 0.051*B(371)

  JVS(174) = -B(316)-B(318)

  JVS(175) = -B(317)+0.051*B(372)+0.093*B(374)

  JVS(176) = 0.099*B(373)

  JVS(177) = 0.108*B(371)

  JVS(178) = -B(313)-B(315)

  JVS(179) = -B(314)+0.108*B(372)+0.099*B(374)

  JVS(180) = 0.187*B(373)

  JVS(181) = 0.207*B(371)

  JVS(182) = -B(269)-B(271)

  JVS(183) = -B(272)

  JVS(184) = -B(270)+0.207*B(372)+0.187*B(374)

  JVS(185) = -B(363)-B(391)

  JVS(186) = -B(392)

  JVS(187) = -B(364)

  JVS(188) = 0.561*B(373)

  JVS(189) = 0.491*B(371)

  JVS(190) = -B(309)-B(311)

  JVS(191) = -B(312)

  JVS(192) = -B(310)+0.491*B(372)+0.561*B(374)

  JVS(193) = B(213)+B(215)

  JVS(194) = -B(273)

  JVS(195) = B(206)

  JVS(196) = B(207)

  JVS(197) = -B(274)

  JVS(198) = B(214)

  JVS(199) = 0.05*B(373)

  JVS(200) = 0.059*B(371)

  JVS(201) = -B(275)-B(277)-B(278)

  JVS(202) = 0.061*B(383)+0.042*B(385)+0.015*B(387)

  JVS(203) = 0.042*B(386)

  JVS(204) = -B(279)+0.015*B(388)

  JVS(205) = -B(276)+0.059*B(372)+0.05*B(374)+0.061*B(384)

  JVS(206) = -B(250)-B(252)

  JVS(207) = B(88)

  JVS(208) = -B(251)

  JVS(209) = B(89)+B(108)

  JVS(210) = B(109)

  JVS(211) = 0.017*B(371)

  JVS(212) = -B(265)-B(267)

  JVS(213) = B(208)+B(210)

  JVS(214) = -B(268)

  JVS(215) = -B(266)+0.017*B(372)

  JVS(216) = B(209)

  JVS(217) = 0.287*B(373)

  JVS(218) = 0.119*B(371)

  JVS(219) = 0.5*B(318)

  JVS(220) = 0.5*B(315)

  JVS(221) = 0.23*B(269)

  JVS(222) = -B(259)-B(260)-B(262)

  JVS(223) = 0.084*B(280)+0.9*B(282)

  JVS(224) = 0.174*B(296)+0.742*B(298)+0.008*B(300)

  JVS(225) = 0.3*B(289)+0.95*B(291)

  JVS(226) = 0.9*B(283)+0.95*B(292)+0.742*B(299)

  JVS(227) = -B(263)+0.008*B(301)

  JVS(228) = -B(261)+0.23*B(270)+0.084*B(281)+0.3*B(290)+0.174*B(297)+0.119*B(372)+0.287*B(374)

  JVS(229) = 0.002*B(367)

  JVS(230) = B(318)

  JVS(231) = B(315)

  JVS(232) = 0.393*B(363)+1.5*B(391)

  JVS(233) = B(309)+1.5*B(311)

  JVS(234) = B(259)+B(260)+B(262)

  JVS(235) = -B(49)

  JVS(236) = 0.5*B(323)+0.491*B(327)

  JVS(237) = 0.51*B(395)

  JVS(238) = 2*B(253)+B(254)+1.26*B(255)+1.26*B(257)

  JVS(239) = 0.275*B(345)

  JVS(240) = 0.157*B(353)

  JVS(241) = 0.157*B(403)

  JVS(242) = 0.416*B(280)+0.45*B(282)+0.5*B(284)+0.67*B(288)

  JVS(243) = 0.345*B(377)

  JVS(244) = 0.336*B(296)+0.498*B(298)+0.572*B(300)+1.233*B(302)

  JVS(245) = 0.265*B(385)+0.012*B(389)

  JVS(246) = 0.475*B(291)+0.7*B(295)

  JVS(247) = B(229)

  JVS(248) = B(216)+B(217)+B(218)+B(225)

  JVS(249) = 0.491*B(328)+0.012*B(390)

  JVS(250) = 0.034*B(232)+B(234)

  JVS(251) = 0.45*B(283)+0.475*B(292)+0.498*B(299)+1.5*B(312)+0.5*B(324)+0.275*B(346)+0.157*B(354)+0.345*B(378)+0.265&
               &*B(386)+1.5*B(392)+0.51*B(396)+0.157*B(404)

  JVS(252) = B(226)+1.26*B(258)+B(263)+0.5*B(285)+0.572*B(301)

  JVS(253) = -B(50)+B(219)+0.034*B(233)+1.26*B(256)+B(261)+0.416*B(281)+0.336*B(297)+B(310)+0.393*B(364)+0.002*B(368)

  JVS(254) = 2*B(24)

  JVS(255) = B(271)

  JVS(256) = B(273)

  JVS(257) = B(278)

  JVS(258) = B(267)

  JVS(259) = B(262)

  JVS(260) = -B(46)-B(48)

  JVS(261) = B(257)

  JVS(262) = 0

  JVS(263) = 0.5*B(284)

  JVS(264) = 0.15*B(300)

  JVS(265) = 0

  JVS(266) = 0

  JVS(267) = B(230)

  JVS(268) = B(225)

  JVS(269) = B(235)

  JVS(270) = 0

  JVS(271) = B(42)

  JVS(272) = 0.2*B(66)+B(226)+B(231)+B(236)+B(258)+B(263)+B(268)+B(272)+B(274)+B(279)+0.5*B(285)+0.15*B(301)

  JVS(273) = B(43)-B(47)

  JVS(274) = 0.2*B(67)

  JVS(275) = -B(321)-B(323)-B(325)-B(327)

  JVS(276) = -B(328)

  JVS(277) = -B(324)

  JVS(278) = -B(326)

  JVS(279) = -B(322)

  JVS(280) = 0.704*B(361)

  JVS(281) = 0.024*B(365)

  JVS(282) = B(205)

  JVS(283) = 0.072*B(369)

  JVS(284) = 0.452*B(367)

  JVS(285) = -B(237)-B(239)

  JVS(286) = 0.13*B(353)

  JVS(287) = 0.13*B(403)

  JVS(288) = 0.005*B(375)+0.001*B(377)+0.024*B(379)

  JVS(289) = 0.127*B(383)+0.045*B(385)+0.102*B(387)

  JVS(290) = 0.006*B(306)+0.02*B(308)

  JVS(291) = 0.13*B(354)+0.001*B(378)+0.045*B(386)+0.13*B(404)

  JVS(292) = 0

  JVS(293) = 0.024*B(380)+0.102*B(388)

  JVS(294) = -B(238)+0.006*B(307)+0.704*B(362)+0.024*B(366)+0.452*B(368)+0.072*B(370)+0.005*B(376)+0.127*B(384)

  JVS(295) = -B(393)-B(395)-B(397)-B(399)

  JVS(296) = -B(400)

  JVS(297) = -B(396)

  JVS(298) = -B(398)

  JVS(299) = -B(394)

  JVS(300) = 0.097*B(373)

  JVS(301) = 0.118*B(371)

  JVS(302) = 0.5*B(318)

  JVS(303) = 0.5*B(315)

  JVS(304) = 0.607*B(363)

  JVS(305) = B(311)

  JVS(306) = 0.23*B(265)

  JVS(307) = 0.009*B(327)

  JVS(308) = -B(253)-B(254)-B(255)-B(257)

  JVS(309) = 0

  JVS(310) = 0.001*B(353)

  JVS(311) = 0.001*B(403)

  JVS(312) = 0.15*B(296)+0.023*B(298)

  JVS(313) = 0.009*B(328)

  JVS(314) = 0.023*B(299)+B(312)+0.001*B(354)+0.001*B(404)

  JVS(315) = -B(258)

  JVS(316) = -B(256)+0.23*B(266)+0.15*B(297)+0.607*B(364)+0.118*B(372)+0.097*B(374)

  JVS(317) = 0

  JVS(318) = 0.24*B(269)+B(271)

  JVS(319) = 0.24*B(265)+B(267)

  JVS(320) = -B(206)-B(208)-B(210)

  JVS(321) = B(200)

  JVS(322) = B(174)

  JVS(323) = B(160)

  JVS(324) = -B(207)

  JVS(325) = B(164)+B(268)+B(272)

  JVS(326) = 0.24*B(266)+0.24*B(270)

  JVS(327) = B(176)

  JVS(328) = B(161)+B(165)+B(175)+B(177)+2*B(178)+B(201)

  JVS(329) = -B(209)

  JVS(330) = -B(329)-B(345)-B(347)-B(349)

  JVS(331) = -B(350)

  JVS(332) = -B(346)

  JVS(333) = -B(348)

  JVS(334) = -B(330)

  JVS(335) = -B(351)-B(353)-B(355)-B(357)

  JVS(336) = -B(358)

  JVS(337) = -B(354)

  JVS(338) = -B(356)

  JVS(339) = -B(352)

  JVS(340) = 0.559*B(365)

  JVS(341) = 0.948*B(369)

  JVS(342) = 0.936*B(367)

  JVS(343) = B(316)+B(318)

  JVS(344) = B(313)+B(315)

  JVS(345) = B(237)

  JVS(346) = 0.079*B(329)+0.126*B(345)+0.187*B(347)+0.24*B(349)

  JVS(347) = 0.5*B(351)+0.729*B(353)+0.75*B(355)

  JVS(348) = -B(95)-B(97)-B(99)-B(101)-B(103)-B(116)-B(132)-B(150)-B(170)-B(192)

  JVS(349) = 0.5*B(401)+0.729*B(403)+0.75*B(405)

  JVS(350) = 0.205*B(375)+0.488*B(379)

  JVS(351) = 0.001*B(383)+0.137*B(385)+0.711*B(387)

  JVS(352) = 0.675*B(289)

  JVS(353) = 0.596*B(306)+0.152*B(308)

  JVS(354) = 0.24*B(350)

  JVS(355) = 0.616*B(240)

  JVS(356) = 0.515*B(305)

  JVS(357) = 0.126*B(346)+0.729*B(354)+0.137*B(386)+0.729*B(404)

  JVS(358) = -B(193)+B(200)

  JVS(359) = -B(133)+B(174)

  JVS(360) = -B(104)

  JVS(361) = -B(96)+B(160)

  JVS(362) = 0

  JVS(363) = -B(100)+B(164)+0.187*B(348)+0.75*B(356)+0.488*B(380)+0.711*B(388)+0.75*B(406)

  JVS(364) = -B(102)

  JVS(365) = B(238)+0.616*B(241)+0.675*B(290)+0.596*B(307)+B(314)+B(317)+0.079*B(330)+0.5*B(352)+0.559*B(366)+0.936&
               &*B(368)+0.948*B(370)+0.205*B(376)+0.001*B(384)+0.5*B(402)

  JVS(366) = -B(151)+B(176)

  JVS(367) = B(161)+B(165)-B(171)+B(175)+B(177)+2*B(178)+B(201)

  JVS(368) = -B(98)

  JVS(369) = -B(117)

  JVS(370) = -B(401)-B(403)-B(405)-B(407)

  JVS(371) = -B(408)

  JVS(372) = -B(404)

  JVS(373) = -B(406)

  JVS(374) = -B(402)

  JVS(375) = 0.23*B(329)+0.39*B(345)

  JVS(376) = -B(280)-B(282)-B(284)-B(286)-B(288)

  JVS(377) = 0.025*B(383)+0.026*B(385)+0.012*B(389)

  JVS(378) = -B(287)+0.012*B(390)

  JVS(379) = -B(283)+0.39*B(346)+0.026*B(386)

  JVS(380) = -B(285)

  JVS(381) = -B(281)+0.23*B(330)+0.025*B(384)

  JVS(382) = -B(375)-B(377)-B(379)-B(381)

  JVS(383) = -B(382)

  JVS(384) = -B(378)

  JVS(385) = -B(380)

  JVS(386) = -B(376)

  JVS(387) = 0.357*B(329)+0.936*B(347)

  JVS(388) = -B(296)-B(298)-B(300)-B(302)

  JVS(389) = 0.025*B(383)

  JVS(390) = 0

  JVS(391) = -B(299)

  JVS(392) = -B(301)+0.936*B(348)

  JVS(393) = -B(297)+0.357*B(330)+0.025*B(384)

  JVS(394) = -B(383)-B(385)-B(387)-B(389)

  JVS(395) = -B(390)

  JVS(396) = -B(386)

  JVS(397) = -B(388)

  JVS(398) = -B(384)

  JVS(399) = 0.32*B(329)+0.16*B(345)

  JVS(400) = 0.019*B(385)+0.048*B(387)

  JVS(401) = -B(289)-B(291)-B(293)-B(295)

  JVS(402) = -B(294)

  JVS(403) = -B(292)+0.16*B(346)+0.019*B(386)

  JVS(404) = 0.048*B(388)

  JVS(405) = -B(290)+0.32*B(330)

  JVS(406) = B(359)

  JVS(407) = 0.96*B(245)

  JVS(408) = 0.445*B(365)

  JVS(409) = 0.099*B(369)

  JVS(410) = 0.455*B(367)

  JVS(411) = 0.195*B(321)+0.25*B(327)

  JVS(412) = 0.984*B(393)+0.5*B(395)

  JVS(413) = 0.294*B(375)+0.154*B(377)+0.009*B(379)

  JVS(414) = 0.129*B(296)+0.047*B(298)+0.467*B(302)

  JVS(415) = 0.732*B(383)+0.456*B(385)+0.507*B(387)

  JVS(416) = -B(227)-B(229)-B(230)

  JVS(417) = 0.439*B(306)+0.431*B(308)

  JVS(418) = 0.25*B(328)

  JVS(419) = 0.034*B(232)+B(234)

  JVS(420) = 0.482*B(240)+B(242)

  JVS(421) = 0.084*B(303)+0.246*B(305)

  JVS(422) = 0.047*B(299)+0.154*B(378)+0.456*B(386)+0.5*B(396)

  JVS(423) = B(198)

  JVS(424) = B(154)

  JVS(425) = B(140)

  JVS(426) = B(144)-B(231)+0.009*B(380)+0.507*B(388)

  JVS(427) = -B(228)+0.034*B(233)+0.482*B(241)+0.96*B(246)+0.129*B(297)+0.084*B(304)+0.439*B(307)+0.195*B(322)+B(360)&
               &+0.445*B(366)+0.455*B(368)+0.099*B(370)+0.294*B(376)+0.732*B(384)+0.984*B(394)

  JVS(428) = B(141)+B(145)+B(155)+2*B(156)+B(176)+B(199)

  JVS(429) = B(177)

  JVS(430) = 0.081*B(245)

  JVS(431) = 0.026*B(365)

  JVS(432) = 0.026*B(369)

  JVS(433) = B(243)

  JVS(434) = 0.35*B(247)+B(249)

  JVS(435) = B(222)

  JVS(436) = 0.024*B(367)

  JVS(437) = 0.096*B(363)

  JVS(438) = 1.61*B(321)+B(323)+0.191*B(327)

  JVS(439) = B(237)

  JVS(440) = 0.984*B(393)+0.5*B(395)

  JVS(441) = B(254)

  JVS(442) = 0

  JVS(443) = 0.624*B(329)+0.592*B(345)+0.24*B(349)

  JVS(444) = 0.276*B(351)+0.235*B(353)

  JVS(445) = 0.276*B(401)+0.235*B(403)

  JVS(446) = 0.084*B(280)+0.2*B(282)+0.67*B(288)

  JVS(447) = 0.732*B(375)+0.5*B(377)

  JVS(448) = 0.055*B(296)+0.125*B(298)+0.227*B(300)+0.3*B(302)

  JVS(449) = 0.244*B(383)+0.269*B(385)+0.079*B(387)

  JVS(450) = 0.3*B(289)+0.1*B(291)

  JVS(451) = -B(216)-B(217)-B(218)-B(220)-B(225)

  JVS(452) = 0.01*B(306)+0.134*B(308)

  JVS(453) = 0.191*B(328)+0.24*B(350)

  JVS(454) = 0.115*B(240)

  JVS(455) = 0.213*B(303)+0.506*B(305)

  JVS(456) = 0.2*B(283)+0.1*B(292)+0.125*B(299)+B(324)+0.592*B(346)+0.235*B(354)+0.5*B(378)+0.269*B(386)+0.5*B(396)&
               &+0.235*B(404)

  JVS(457) = B(182)+B(186)+B(188)+B(196)+B(198)+B(200)+2*B(202)

  JVS(458) = B(128)+B(197)

  JVS(459) = 0.75*B(92)

  JVS(460) = B(78)+B(183)

  JVS(461) = 0

  JVS(462) = B(82)+B(187)-B(226)+0.227*B(301)+0.079*B(388)

  JVS(463) = B(79)+B(83)+B(84)+2*B(85)+0.75*B(93)+0.75*B(110)+B(129)+B(146)+B(166)+B(189)

  JVS(464) = -B(219)+B(238)+0.115*B(241)+B(244)+0.081*B(246)+0.35*B(248)+0.084*B(281)+0.3*B(290)+0.055*B(297)+0.213&
               &*B(304)+0.01*B(307)+1.61*B(322)+0.624*B(330)+0.276*B(352)+0.096*B(364)+0.026*B(366)+0.024*B(368)+0.026&
               &*B(370)+0.732*B(376)+0.244*B(384)+0.984*B(394)+0.276*B(402)

  JVS(465) = B(147)+B(199)

  JVS(466) = B(167)+B(201)

  JVS(467) = -B(221)

  JVS(468) = 0.75*B(111)

  JVS(469) = B(203)

  JVS(470) = 0.276*B(355)

  JVS(471) = 0.276*B(405)

  JVS(472) = 0.511*B(379)

  JVS(473) = 0.572*B(300)

  JVS(474) = 0.321*B(387)

  JVS(475) = -0.69*B(306)-B(308)

  JVS(476) = 0

  JVS(477) = 0

  JVS(478) = B(106)

  JVS(479) = B(204)

  JVS(480) = 0.572*B(301)+0.276*B(356)+0.511*B(380)+0.321*B(388)+0.276*B(406)

  JVS(481) = -0.69*B(307)

  JVS(482) = B(107)

  JVS(483) = B(34)

  JVS(484) = -B(327)

  JVS(485) = -B(399)

  JVS(486) = -B(349)

  JVS(487) = -B(357)

  JVS(488) = -B(407)

  JVS(489) = -B(286)

  JVS(490) = -B(381)

  JVS(491) = -B(389)

  JVS(492) = -B(293)

  JVS(493) = -B(2)-B(4)-B(6)-B(9)-B(11)-B(287)-B(294)-B(328)-B(350)-B(358)-B(382)-B(390)-B(400)-B(408)

  JVS(494) = -B(5)+B(30)

  JVS(495) = -B(7)

  JVS(496) = B(1)-B(10)-B(12)

  JVS(497) = B(29)

  JVS(498) = 0

  JVS(499) = 0.261*B(361)

  JVS(500) = 0.122*B(365)

  JVS(501) = 0.204*B(369)

  JVS(502) = 0.244*B(367)

  JVS(503) = B(316)

  JVS(504) = B(313)

  JVS(505) = B(309)

  JVS(506) = B(250)+B(252)

  JVS(507) = B(325)

  JVS(508) = 0.45*B(399)

  JVS(509) = 0.474*B(351)+0.205*B(353)+0.474*B(355)+0.147*B(357)

  JVS(510) = 0.474*B(401)+0.205*B(403)+0.474*B(405)+0.147*B(407)

  JVS(511) = B(286)

  JVS(512) = 0.497*B(375)+0.363*B(377)+0.037*B(379)+0.45*B(381)

  JVS(513) = 0.013*B(296)+0.218*B(300)

  JVS(514) = 0.511*B(383)+0.305*B(385)+0.151*B(387)+0.069*B(389)

  JVS(515) = 0.675*B(289)+0.45*B(293)

  JVS(516) = 0.213*B(306)+0.147*B(308)

  JVS(517) = B(287)+0.45*B(294)+0.147*B(358)+0.45*B(382)+0.069*B(390)+0.45*B(400)+0.147*B(408)

  JVS(518) = -B(232)-B(234)-B(235)

  JVS(519) = 0.37*B(240)

  JVS(520) = 0.558*B(303)+0.71*B(305)

  JVS(521) = 0.205*B(354)+0.363*B(378)+0.305*B(386)+0.205*B(404)

  JVS(522) = 0

  JVS(523) = 0

  JVS(524) = 0

  JVS(525) = -B(236)+0.218*B(301)+B(326)+0.474*B(356)+0.037*B(380)+0.151*B(388)+0.474*B(406)

  JVS(526) = -B(233)+0.37*B(241)+B(251)+0.675*B(290)+0.013*B(297)+0.558*B(304)+0.213*B(307)+B(310)+B(314)+B(317)+0.474&
               &*B(352)+0.261*B(362)+0.122*B(366)+0.244*B(368)+0.204*B(370)+0.497*B(376)+0.511*B(384)+0.474*B(402)

  JVS(527) = 0

  JVS(528) = 0

  JVS(529) = 0.332*B(365)

  JVS(530) = 0.089*B(369)

  JVS(531) = 0.11*B(367)

  JVS(532) = 0.55*B(399)

  JVS(533) = 0.416*B(280)

  JVS(534) = 0.437*B(381)

  JVS(535) = 0.15*B(296)+0.21*B(298)+0.233*B(302)

  JVS(536) = 0.072*B(383)+0.026*B(385)+0.001*B(387)+0.659*B(389)

  JVS(537) = 0.55*B(293)

  JVS(538) = 0.177*B(306)+0.243*B(308)

  JVS(539) = 0.55*B(294)+0.437*B(382)+0.659*B(390)+0.55*B(400)

  JVS(540) = -B(240)-B(242)

  JVS(541) = 0.115*B(303)

  JVS(542) = 0.21*B(299)+0.026*B(386)

  JVS(543) = 0.5*B(114)

  JVS(544) = 0

  JVS(545) = 0

  JVS(546) = B(112)+0.001*B(388)

  JVS(547) = 0.5*B(110)

  JVS(548) = -B(241)+0.416*B(281)+0.15*B(297)+0.115*B(304)+0.177*B(307)+0.332*B(366)+0.11*B(368)+0.089*B(370)+0.072&
               &*B(384)

  JVS(549) = 0.5*B(111)+B(113)+0.5*B(115)+B(118)

  JVS(550) = 0.417*B(369)

  JVS(551) = 0.125*B(367)

  JVS(552) = 0.055*B(371)

  JVS(553) = 0.1*B(345)+0.75*B(349)

  JVS(554) = 0.276*B(351)+0.276*B(353)+0.853*B(357)

  JVS(555) = 0.276*B(401)+0.276*B(403)+0.853*B(407)

  JVS(556) = 0.119*B(375)+0.215*B(377)+0.113*B(381)

  JVS(557) = 0.332*B(296)

  JVS(558) = 0.043*B(385)+0.259*B(389)

  JVS(559) = 0.7*B(295)

  JVS(560) = 0.048*B(306)+0.435*B(308)

  JVS(561) = 0.75*B(350)+0.853*B(358)+0.113*B(382)+0.259*B(390)+0.853*B(408)

  JVS(562) = -0.671*B(303)-B(305)

  JVS(563) = 0.1*B(346)+0.276*B(354)+0.215*B(378)+0.043*B(386)+0.276*B(404)

  JVS(564) = B(134)

  JVS(565) = 0.5*B(114)

  JVS(566) = 0

  JVS(567) = 0

  JVS(568) = 0

  JVS(569) = 0.5*B(110)

  JVS(570) = 0.332*B(297)-0.671*B(304)+0.048*B(307)+0.276*B(352)+0.125*B(368)+0.417*B(370)+0.055*B(372)+0.119*B(376)&
               &+0.276*B(402)

  JVS(571) = B(152)

  JVS(572) = B(172)

  JVS(573) = 0.5*B(111)+0.5*B(115)+B(118)+B(135)+B(153)+B(173)

  JVS(574) = -B(391)

  JVS(575) = -B(311)

  JVS(576) = -B(323)

  JVS(577) = -B(395)

  JVS(578) = -B(345)

  JVS(579) = -B(353)

  JVS(580) = -B(403)

  JVS(581) = -B(282)

  JVS(582) = -B(377)

  JVS(583) = -B(298)

  JVS(584) = -B(385)

  JVS(585) = -B(291)

  JVS(586) = B(2)-B(4)

  JVS(587) = -B(5)-B(13)-B(15)-B(30)-B(31)-B(51)-B(61)-B(283)-B(292)-B(299)-B(312)-B(324)-B(346)-B(354)-B(378)-B(386)&
               &-B(392)-B(396)-B(404)

  JVS(588) = 0.25*B(184)

  JVS(589) = 0.25*B(124)

  JVS(590) = -B(14)

  JVS(591) = -B(16)

  JVS(592) = 0

  JVS(593) = -B(52)

  JVS(594) = 0.25*B(142)

  JVS(595) = 0.25*B(162)

  JVS(596) = -B(62)+0.25*B(125)+0.25*B(143)+0.25*B(163)+0.25*B(185)

  JVS(597) = B(181)

  JVS(598) = 0.192*B(345)+0.24*B(349)

  JVS(599) = 0.5*B(280)+0.5*B(284)+0.33*B(288)

  JVS(600) = 0.289*B(296)+0.15*B(300)

  JVS(601) = 0

  JVS(602) = 0.3*B(295)

  JVS(603) = 0.24*B(350)

  JVS(604) = 0.192*B(346)

  JVS(605) = -B(179)-B(182)-B(184)-B(186)-B(188)-B(190)-B(194)-B(196)-B(198)-B(200)-2*B(202)

  JVS(606) = -B(197)

  JVS(607) = -B(191)

  JVS(608) = -B(183)

  JVS(609) = -B(180)

  JVS(610) = -B(187)+0.5*B(285)+0.15*B(301)

  JVS(611) = -B(189)

  JVS(612) = 0.5*B(281)+0.289*B(297)

  JVS(613) = -B(199)

  JVS(614) = -B(201)

  JVS(615) = -B(185)

  JVS(616) = -B(195)

  JVS(617) = B(121)

  JVS(618) = 2*B(264)

  JVS(619) = 0

  JVS(620) = 0.011*B(367)

  JVS(621) = B(316)+0.5*B(318)

  JVS(622) = B(313)+0.5*B(315)

  JVS(623) = B(259)+B(260)+B(262)

  JVS(624) = B(237)+B(239)

  JVS(625) = 0.123*B(353)

  JVS(626) = 0.123*B(403)

  JVS(627) = 0.67*B(288)

  JVS(628) = 0

  JVS(629) = 0.467*B(302)

  JVS(630) = 0.137*B(385)

  JVS(631) = 0.675*B(289)

  JVS(632) = B(227)+B(230)

  JVS(633) = 0

  JVS(634) = 0

  JVS(635) = 0

  JVS(636) = 0.492*B(240)+B(242)

  JVS(637) = 0.029*B(303)+0.667*B(305)

  JVS(638) = 0.123*B(354)+0.137*B(386)+0.123*B(404)

  JVS(639) = B(182)+B(186)+B(198)+B(200)+2*B(202)

  JVS(640) = -B(119)-B(122)-B(124)-B(126)-B(128)-B(130)-B(134)-2*B(136)-B(154)-B(174)

  JVS(641) = -B(131)

  JVS(642) = -B(123)+B(183)

  JVS(643) = -B(120)

  JVS(644) = -B(127)+B(187)+B(231)+B(263)

  JVS(645) = -B(129)

  JVS(646) = B(228)+B(238)+0.492*B(241)+B(261)+0.675*B(290)+0.029*B(304)+B(314)+B(317)+0.011*B(368)

  JVS(647) = -B(155)+B(199)

  JVS(648) = -B(175)+B(201)

  JVS(649) = -B(125)

  JVS(650) = -B(135)

  JVS(651) = B(359)

  JVS(652) = 0.05*B(245)

  JVS(653) = 0.965*B(361)

  JVS(654) = 0.695*B(365)

  JVS(655) = 0.653*B(369)

  JVS(656) = 0.804*B(373)

  JVS(657) = 0.835*B(367)

  JVS(658) = 0.765*B(371)

  JVS(659) = B(318)

  JVS(660) = B(315)

  JVS(661) = 0.76*B(269)

  JVS(662) = 0.1*B(363)

  JVS(663) = B(309)

  JVS(664) = 0.34*B(250)

  JVS(665) = 0.76*B(265)

  JVS(666) = B(321)+B(325)+0.2*B(327)

  JVS(667) = 0.984*B(393)+0.949*B(397)

  JVS(668) = 0

  JVS(669) = 0.907*B(329)+0.066*B(345)+0.749*B(347)

  JVS(670) = 0.75*B(351)+0.031*B(353)+0.276*B(355)

  JVS(671) = 0.75*B(401)+0.031*B(403)+0.276*B(405)

  JVS(672) = 0.5*B(280)+0.1*B(282)+0.5*B(284)+0.33*B(288)

  JVS(673) = 0.91*B(375)+0.022*B(377)+0.824*B(379)

  JVS(674) = 0.67*B(296)+0.048*B(298)+0.799*B(300)

  JVS(675) = 0.918*B(383)+0.033*B(385)+0.442*B(387)+0.012*B(389)

  JVS(676) = 0.3*B(289)+0.05*B(291)

  JVS(677) = 0.376*B(306)+0.564*B(308)

  JVS(678) = 0.2*B(328)+0.012*B(390)

  JVS(679) = 0.034*B(232)+B(234)

  JVS(680) = 0.37*B(240)+B(242)

  JVS(681) = 0.473*B(303)+0.96*B(305)

  JVS(682) = 0.1*B(283)+0.05*B(292)+0.048*B(299)+0.066*B(346)+0.031*B(354)+0.022*B(378)+0.033*B(386)+0.031*B(404)

  JVS(683) = -B(190)+B(198)

  JVS(684) = -B(130)+B(154)

  JVS(685) = -B(86)-B(88)-B(90)-B(92)-2*B(94)-B(114)-B(131)-B(148)-B(168)-B(191)

  JVS(686) = -B(87)+B(140)

  JVS(687) = 0

  JVS(688) = -B(91)+B(144)+0.5*B(285)+0.799*B(301)+B(326)+0.749*B(348)+0.276*B(356)+0.824*B(380)+0.442*B(388)+0.949&
               &*B(398)+0.276*B(406)

  JVS(689) = -B(93)

  JVS(690) = 0.034*B(233)+0.37*B(241)+0.05*B(246)+0.34*B(251)+0.76*B(266)+0.76*B(270)+0.5*B(281)+0.3*B(290)+0.67*B(297)&
               &+0.473*B(304)+0.376*B(307)+B(310)+B(322)+0.907*B(330)+0.75*B(352)+B(360)+0.965*B(362)+0.1*B(364)+0.695&
               &*B(366)+0.835*B(368)+0.653*B(370)+0.765*B(372)+0.804*B(374)+0.91*B(376)+0.918*B(384)+0.984*B(394)+0.75&
               &*B(402)

  JVS(691) = B(141)+B(145)-B(149)+B(155)+2*B(156)+B(176)+B(199)

  JVS(692) = -B(169)+B(177)

  JVS(693) = -B(89)

  JVS(694) = -B(115)

  JVS(695) = B(38)

  JVS(696) = -B(223)

  JVS(697) = -B(95)

  JVS(698) = 0

  JVS(699) = 0

  JVS(700) = 0

  JVS(701) = 0

  JVS(702) = 0

  JVS(703) = 0

  JVS(704) = -B(6)+B(9)

  JVS(705) = 0

  JVS(706) = 0

  JVS(707) = -B(13)

  JVS(708) = -B(182)

  JVS(709) = -B(122)

  JVS(710) = -B(86)

  JVS(711) = -B(7)-B(14)-B(17)-2*B(19)-B(36)-B(53)-B(78)-B(87)-B(96)-B(106)-B(123)-B(140)-B(160)-B(183)-B(224)

  JVS(712) = B(1)+B(10)+B(26)

  JVS(713) = -B(18)+B(27)+B(28)

  JVS(714) = -B(79)

  JVS(715) = -B(37)

  JVS(716) = -B(141)

  JVS(717) = -B(161)

  JVS(718) = -B(54)

  JVS(719) = -B(107)

  JVS(720) = B(121)

  JVS(721) = B(139)

  JVS(722) = B(159)

  JVS(723) = B(181)

  JVS(724) = B(23)

  JVS(725) = B(39)+B(40)

  JVS(726) = -B(203)

  JVS(727) = B(223)

  JVS(728) = -B(211)

  JVS(729) = B(57)+0.61*B(58)+B(59)

  JVS(730) = 0

  JVS(731) = B(48)

  JVS(732) = 0

  JVS(733) = -B(206)

  JVS(734) = 0.187*B(347)

  JVS(735) = 0.474*B(355)

  JVS(736) = B(95)+B(99)

  JVS(737) = 0.474*B(405)

  JVS(738) = 0

  JVS(739) = 0

  JVS(740) = 0

  JVS(741) = 0.391*B(387)

  JVS(742) = 0

  JVS(743) = 0

  JVS(744) = 0

  JVS(745) = 0.338*B(306)+B(308)

  JVS(746) = B(6)-B(9)-B(11)

  JVS(747) = 0

  JVS(748) = 0

  JVS(749) = 0

  JVS(750) = B(13)-B(15)

  JVS(751) = -B(179)+B(182)+B(186)

  JVS(752) = -B(119)+B(122)+B(126)

  JVS(753) = B(86)+B(90)

  JVS(754) = B(7)+B(14)+2*B(17)+2*B(19)+B(53)+B(78)+B(87)+B(96)+B(123)+B(140)+B(160)+B(183)+B(224)

  JVS(755) = -B(1)-B(10)-B(12)-B(16)-B(21)-B(42)-B(55)-B(120)-B(137)-B(157)-B(180)-B(204)-B(207)-B(212)

  JVS(756) = 2*B(18)-B(22)+B(29)+B(44)+0.8*B(66)+2*B(68)+B(82)+B(91)+B(100)+B(112)+B(127)+B(144)+B(164)+B(187)+0.187&
               &*B(348)+0.474*B(356)+0.391*B(388)+0.474*B(406)

  JVS(757) = B(79)+B(83)

  JVS(758) = B(41)-B(43)+B(45)+B(60)+0.338*B(307)

  JVS(759) = -B(138)+B(141)+B(145)

  JVS(760) = -B(158)+B(161)+B(165)

  JVS(761) = B(54)-B(56)+0.8*B(67)

  JVS(762) = B(113)

  JVS(763) = B(23)

  JVS(764) = 0.39*B(58)

  JVS(765) = -B(271)

  JVS(766) = -B(273)

  JVS(767) = -B(278)

  JVS(768) = -B(267)

  JVS(769) = -B(262)

  JVS(770) = B(46)

  JVS(771) = -B(325)

  JVS(772) = -B(397)

  JVS(773) = -B(257)

  JVS(774) = 0

  JVS(775) = -B(347)

  JVS(776) = -B(355)

  JVS(777) = -B(99)

  JVS(778) = -B(405)

  JVS(779) = -B(284)

  JVS(780) = -B(379)

  JVS(781) = -B(300)

  JVS(782) = -B(387)

  JVS(783) = 0

  JVS(784) = -B(230)

  JVS(785) = -B(225)

  JVS(786) = 0

  JVS(787) = B(11)

  JVS(788) = -B(235)

  JVS(789) = 0

  JVS(790) = 0

  JVS(791) = B(15)

  JVS(792) = -B(186)

  JVS(793) = -B(126)

  JVS(794) = -B(90)

  JVS(795) = -B(17)

  JVS(796) = B(12)+B(16)-B(21)-B(26)

  JVS(797) = -B(18)-B(22)-B(27)-B(28)-B(29)-B(44)-B(66)-2*B(68)-B(82)-B(91)-B(100)-B(112)-B(127)-B(144)-B(164)-B(187)&
               &-B(226)-B(231)-B(236)-B(258)-B(263)-B(268)-B(272)-B(274)-B(279)-B(285)-B(301)-B(326)-B(348)-B(356)-B(380)&
               &-B(388)-B(398)-B(406)

  JVS(798) = -B(83)

  JVS(799) = -B(45)+B(47)

  JVS(800) = -B(145)

  JVS(801) = -B(165)

  JVS(802) = -B(67)

  JVS(803) = -B(113)

  JVS(804) = B(319)

  JVS(805) = B(205)

  JVS(806) = 0.65*B(247)

  JVS(807) = 0.011*B(367)

  JVS(808) = 0.3*B(327)

  JVS(809) = B(239)

  JVS(810) = 0.26*B(395)

  JVS(811) = 0.25*B(349)

  JVS(812) = 0

  JVS(813) = 0

  JVS(814) = 0.076*B(377)

  JVS(815) = 0.197*B(385)+0.03*B(387)

  JVS(816) = 0.3*B(295)

  JVS(817) = B(229)

  JVS(818) = 0

  JVS(819) = 0.3*B(328)+0.25*B(350)

  JVS(820) = 0

  JVS(821) = 0

  JVS(822) = 0

  JVS(823) = 0.076*B(378)+0.197*B(386)+0.26*B(396)

  JVS(824) = -B(188)+B(196)

  JVS(825) = B(122)+B(126)-B(128)+2*B(136)+B(154)+B(174)+B(197)

  JVS(826) = -B(92)

  JVS(827) = -B(78)+B(123)

  JVS(828) = 0

  JVS(829) = -B(82)+B(127)+0.03*B(388)

  JVS(830) = -B(79)-B(80)-B(83)-2*B(84)-2*B(85)-B(93)-B(110)-B(129)-B(146)-B(166)-B(189)

  JVS(831) = 0.65*B(248)+B(320)+0.011*B(368)

  JVS(832) = -B(147)+B(155)

  JVS(833) = -B(167)+B(175)

  JVS(834) = -B(81)

  JVS(835) = -B(111)

  JVS(836) = 2*B(32)

  JVS(837) = -B(319)

  JVS(838) = -B(74)

  JVS(839) = -B(359)

  JVS(840) = 2*B(69)-B(70)

  JVS(841) = -B(245)

  JVS(842) = -B(361)

  JVS(843) = B(38)-B(40)

  JVS(844) = -B(365)

  JVS(845) = -B(369)

  JVS(846) = -B(243)

  JVS(847) = -B(373)

  JVS(848) = -0.65*B(247)+B(249)

  JVS(849) = 0.39*B(58)-B(59)

  JVS(850) = -B(367)

  JVS(851) = -B(371)

  JVS(852) = -B(316)

  JVS(853) = -B(313)

  JVS(854) = -B(269)

  JVS(855) = -0.397*B(363)+0.5*B(391)

  JVS(856) = -B(309)+0.5*B(311)

  JVS(857) = -B(275)

  JVS(858) = -0.34*B(250)+B(252)

  JVS(859) = -B(265)

  JVS(860) = -B(260)

  JVS(861) = -B(49)

  JVS(862) = -B(46)+B(48)

  JVS(863) = -B(321)+0.12*B(323)

  JVS(864) = -B(237)

  JVS(865) = -B(393)+0.32*B(395)

  JVS(866) = -B(255)

  JVS(867) = 0

  JVS(868) = -B(329)+0.266*B(345)

  JVS(869) = -B(351)+0.567*B(353)

  JVS(870) = -B(401)+0.567*B(403)

  JVS(871) = -B(280)+0.208*B(282)+0.33*B(288)

  JVS(872) = -B(375)+0.155*B(377)

  JVS(873) = -B(296)+0.285*B(298)

  JVS(874) = -B(383)+0.378*B(385)

  JVS(875) = -B(289)+0.164*B(291)

  JVS(876) = -B(227)

  JVS(877) = -B(218)

  JVS(878) = -B(306)

  JVS(879) = 0

  JVS(880) = -B(232)

  JVS(881) = -B(240)

  JVS(882) = -B(303)

  JVS(883) = -B(51)+B(61)+0.208*B(283)+0.164*B(292)+0.285*B(299)+0.5*B(312)+0.12*B(324)+0.266*B(346)+0.567*B(354)+0.155&
               &*B(378)+0.378*B(386)+0.5*B(392)+0.32*B(396)+0.567*B(404)

  JVS(884) = 0

  JVS(885) = 0

  JVS(886) = 0

  JVS(887) = -B(36)+B(53)

  JVS(888) = -B(42)

  JVS(889) = -B(44)+0.8*B(66)

  JVS(890) = 0

  JVS(891) = -B(37)-B(41)-B(43)-B(45)-B(47)-B(50)-B(52)-B(60)-B(71)-B(72)-B(75)-B(76)-B(219)-B(228)-B(233)-B(238)-B(241)&
               &-B(244)-B(246)-0.65*B(248)-0.34*B(251)-B(256)-B(261)-B(266)-B(270)-B(276)-B(281)-B(290)-B(297)-B(304)-B(307)&
               &-B(310)-B(314)-B(317)-B(320)-B(322)-B(330)-B(352)-B(360)-B(362)-0.397*B(364)-B(366)-B(368)-B(370)-B(372)&
               &-B(374)-B(376)-B(384)-B(394)-B(402)

  JVS(892) = 0

  JVS(893) = 0

  JVS(894) = B(54)+B(62)+0.8*B(67)-B(73)

  JVS(895) = 0

  JVS(896) = B(139)

  JVS(897) = 0.37*B(255)+0.37*B(257)

  JVS(898) = 0

  JVS(899) = 0.201*B(353)

  JVS(900) = 0.201*B(403)

  JVS(901) = 0.1*B(282)

  JVS(902) = 0.048*B(298)+0.3*B(302)

  JVS(903) = 0.006*B(385)

  JVS(904) = 0.05*B(291)

  JVS(905) = 0

  JVS(906) = 0.965*B(232)+B(235)

  JVS(907) = 0.096*B(240)

  JVS(908) = 0.049*B(303)+0.333*B(305)

  JVS(909) = 0.1*B(283)+0.05*B(292)+0.048*B(299)+0.201*B(354)+0.006*B(386)+0.201*B(404)

  JVS(910) = -B(198)

  JVS(911) = -B(154)

  JVS(912) = -B(148)

  JVS(913) = -B(140)

  JVS(914) = -B(137)

  JVS(915) = -B(144)+B(236)+0.37*B(258)

  JVS(916) = -B(146)

  JVS(917) = 0.965*B(233)+0.096*B(241)+0.37*B(256)+0.049*B(304)

  JVS(918) = -B(138)-B(141)-B(142)-B(145)-B(147)-B(149)-B(152)-B(155)-2*B(156)-B(176)-B(199)

  JVS(919) = -B(177)

  JVS(920) = -B(143)

  JVS(921) = -B(153)

  JVS(922) = B(159)

  JVS(923) = B(275)+B(278)

  JVS(924) = 0

  JVS(925) = 0

  JVS(926) = 0

  JVS(927) = -B(200)

  JVS(928) = -B(174)

  JVS(929) = -B(168)

  JVS(930) = -B(160)

  JVS(931) = -B(157)

  JVS(932) = -B(164)+B(279)

  JVS(933) = -B(166)

  JVS(934) = B(276)

  JVS(935) = -B(176)

  JVS(936) = -B(158)-B(161)-B(162)-B(165)-B(167)-B(169)-B(172)-B(175)-B(177)-2*B(178)-B(201)

  JVS(937) = -B(163)

  JVS(938) = -B(173)

  JVS(939) = B(74)

  JVS(940) = B(70)

  JVS(941) = 0.95*B(245)

  JVS(942) = B(39)

  JVS(943) = B(243)

  JVS(944) = 0.187*B(373)

  JVS(945) = B(249)

  JVS(946) = B(222)+B(223)

  JVS(947) = -B(213)

  JVS(948) = B(57)+0.61*B(58)

  JVS(949) = 0.224*B(371)

  JVS(950) = 0.5*B(318)

  JVS(951) = 0.5*B(315)

  JVS(952) = 0.297*B(363)+1.5*B(391)

  JVS(953) = 1.5*B(311)

  JVS(954) = 0

  JVS(955) = B(252)

  JVS(956) = B(259)

  JVS(957) = B(49)

  JVS(958) = 0.12*B(323)+0.5*B(327)

  JVS(959) = 0.06*B(395)

  JVS(960) = 2*B(253)+0.63*B(255)+0.63*B(257)

  JVS(961) = -B(208)

  JVS(962) = 0.907*B(329)

  JVS(963) = 0.033*B(353)

  JVS(964) = 0.033*B(403)

  JVS(965) = 0.008*B(282)+0.34*B(288)

  JVS(966) = 0.056*B(377)

  JVS(967) = 0.4*B(298)+1.233*B(302)

  JVS(968) = 0.003*B(385)+0.013*B(389)

  JVS(969) = 0.064*B(291)

  JVS(970) = B(229)

  JVS(971) = 2*B(216)+B(218)-B(220)+B(225)

  JVS(972) = 0.113*B(306)+0.341*B(308)

  JVS(973) = 0.5*B(328)+0.013*B(390)

  JVS(974) = B(234)

  JVS(975) = 0

  JVS(976) = 0.379*B(303)

  JVS(977) = B(51)-B(61)+0.008*B(283)+0.064*B(292)+0.4*B(299)+1.5*B(312)+0.12*B(324)+0.033*B(354)+0.056*B(378)+0.003&
               &*B(386)+1.5*B(392)+0.06*B(396)+0.033*B(404)

  JVS(978) = -B(184)

  JVS(979) = -B(124)

  JVS(980) = B(86)-B(88)+B(90)+B(92)+B(94)+B(114)

  JVS(981) = -B(53)+B(78)+B(87)+B(224)

  JVS(982) = -B(55)

  JVS(983) = B(44)-B(66)+B(82)+B(91)+B(112)+B(226)+0.63*B(258)

  JVS(984) = B(79)-B(80)+B(83)+2*B(85)+B(93)+B(110)

  JVS(985) = B(45)+B(50)+B(52)+B(71)-B(72)+B(75)+B(76)+B(219)+B(244)+0.95*B(246)+0.63*B(256)+0.379*B(304)+0.113*B(307)&
               &+0.907*B(330)+0.297*B(364)+0.224*B(372)+0.187*B(374)

  JVS(986) = -B(142)

  JVS(987) = -B(162)

  JVS(988) = -B(54)-B(56)-B(62)-2*B(63)-2*B(64)-B(67)-B(73)-B(81)-B(89)-B(108)-B(125)-B(143)-B(163)-B(185)-B(209)-B(214)&
               &-B(221)

  JVS(989) = -B(109)+B(111)+B(113)+B(115)+B(118)

  JVS(990) = 0.035*B(361)

  JVS(991) = 0.07*B(365)

  JVS(992) = 0.347*B(369)

  JVS(993) = 0.009*B(373)

  JVS(994) = 0.143*B(367)

  JVS(995) = 0.011*B(371)

  JVS(996) = 0.016*B(393)+0.051*B(397)

  JVS(997) = 0.093*B(329)+0.008*B(345)+0.064*B(347)+0.01*B(349)

  JVS(998) = 0.25*B(351)+0.18*B(353)+0.25*B(355)

  JVS(999) = 0.25*B(401)+0.18*B(403)+0.25*B(405)

  JVS(1000) = 0.09*B(375)+0.001*B(377)+0.176*B(379)

  JVS(1001) = 0.041*B(296)+0.051*B(300)

  JVS(1002) = 0.082*B(383)+0.002*B(385)+0.136*B(387)+0.001*B(389)

  JVS(1003) = 0.025*B(289)

  JVS(1004) = 0.173*B(306)+0.095*B(308)

  JVS(1005) = 0.01*B(350)+0.001*B(390)

  JVS(1006) = 0.001*B(232)

  JVS(1007) = 0.042*B(240)

  JVS(1008) = 0.07*B(303)+0.04*B(305)

  JVS(1009) = 0.008*B(346)+0.18*B(354)+0.001*B(378)+0.002*B(386)+0.18*B(404)

  JVS(1010) = -B(194)

  JVS(1011) = -B(134)

  JVS(1012) = -B(114)

  JVS(1013) = -B(106)

  JVS(1014) = 0

  JVS(1015) = -B(112)+0.051*B(301)+0.064*B(348)+0.25*B(356)+0.176*B(380)+0.136*B(388)+0.051*B(398)+0.25*B(406)

  JVS(1016) = -B(110)

  JVS(1017) = 0.001*B(233)+0.042*B(241)+0.025*B(290)+0.041*B(297)+0.07*B(304)+0.173*B(307)+0.093*B(330)+0.25*B(352)&
                &+0.035*B(362)+0.07*B(366)+0.143*B(368)+0.347*B(370)+0.011*B(372)+0.009*B(374)+0.09*B(376)+0.082*B(384)&
                &+0.016*B(394)+0.25*B(402)

  JVS(1018) = -B(152)

  JVS(1019) = -B(172)

  JVS(1020) = -B(108)

  JVS(1021) = -B(107)-B(109)-B(111)-B(113)-B(115)-2*B(118)-B(135)-B(153)-B(173)-B(195)
      
END SUBROUTINE saprc99_Jac_SP














SUBROUTINE saprc99_KppDecomp( JVS, IER )







      INTEGER  :: IER
      REAL(kind=dp) :: JVS(1021), W(86), a
      INTEGER  :: k, kk, j, jj

      a = 0. 
      IER = 0
      DO k=1,NVAR

        
        IF ( ABS(JVS(LU_DIAG(k))) < TINY(a) ) THEN
            IER = k
            RETURN
        END IF
        DO kk = LU_CROW(k), LU_CROW(k+1)-1
              W( LU_ICOL(kk) ) = JVS(kk)
        END DO
        DO kk = LU_CROW(k), LU_DIAG(k)-1
            j = LU_ICOL(kk)
            a = -W(j) / JVS( LU_DIAG(j) )
            W(j) = -a
            DO jj = LU_DIAG(j)+1, LU_CROW(j+1)-1
               W( LU_ICOL(jj) ) = W( LU_ICOL(jj) ) + a*JVS(jj)
            END DO
         END DO
         DO kk = LU_CROW(k), LU_CROW(k+1)-1
            JVS(kk) = W( LU_ICOL(kk) )
         END DO
      END DO
      
END SUBROUTINE saprc99_KppDecomp



SUBROUTINE saprc99_KppDecompCmplx( JVS, IER )







      INTEGER  :: IER
      DOUBLE COMPLEX :: JVS(1021), W(86), a
      INTEGER  :: k, kk, j, jj

      IER = 0
      DO k=1,NVAR
        IF ( JVS( LU_DIAG(k) ) .EQ. 0. ) THEN
            IER = k
            RETURN
        END IF
        DO kk = LU_CROW(k), LU_CROW(k+1)-1
              W( LU_ICOL(kk) ) = JVS(kk)
        END DO
        DO kk = LU_CROW(k), LU_DIAG(k)-1
            j = LU_ICOL(kk)
            a = -W(j) / JVS( LU_DIAG(j) )
            W(j) = -a
            DO jj = LU_DIAG(j)+1, LU_CROW(j+1)-1
               W( LU_ICOL(jj) ) = W( LU_ICOL(jj) ) + a*JVS(jj)
            END DO
         END DO
         DO kk = LU_CROW(k), LU_CROW(k+1)-1
            JVS(kk) = W( LU_ICOL(kk) )
         END DO
      END DO
      
END SUBROUTINE saprc99_KppDecompCmplx


SUBROUTINE saprc99_KppSolveIndirect( JVS, X )







      INTEGER i, j
      REAL(kind=dp) JVS(1021), X(86), sum

      DO i=1,NVAR
         DO j = LU_CROW(i), LU_DIAG(i)-1 
             X(i) = X(i) - JVS(j)*X(LU_ICOL(j));
         END DO  
      END DO

      DO i=NVAR,1,-1
        sum = X(i);
        DO j = LU_DIAG(i)+1, LU_CROW(i+1)-1
          sum = sum - JVS(j)*X(LU_ICOL(j));
        END DO
        X(i) = sum/JVS(LU_DIAG(i));
      END DO
      
END SUBROUTINE saprc99_KppSolveIndirect


SUBROUTINE saprc99_KppSolveCmplx( JVS, X )







      INTEGER i, j
      DOUBLE COMPLEX JVS(1021), X(86), sum

      DO i=1,NVAR
         DO j = LU_CROW(i), LU_DIAG(i)-1 
             X(i) = X(i) - JVS(j)*X(LU_ICOL(j));
         END DO  
      END DO

      DO i=NVAR,1,-1
        sum = X(i);
        DO j = LU_DIAG(i)+1, LU_CROW(i+1)-1
          sum = sum - JVS(j)*X(LU_ICOL(j));
        END DO
        X(i) = sum/JVS(LU_DIAG(i));
      END DO
      
END SUBROUTINE saprc99_KppSolveCmplx













SUBROUTINE saprc99_KppSolve ( JVS, X )


  REAL(kind=dp) :: JVS(LU_NONZERO)

  REAL(kind=dp) :: X(NVAR)

  X(18) = X(18)-JVS(97)*X(17)
  X(31) = X(31)-JVS(138)*X(30)
  X(41) = X(41)-JVS(172)*X(34)-JVS(173)*X(40)
  X(42) = X(42)-JVS(176)*X(34)-JVS(177)*X(40)
  X(43) = X(43)-JVS(180)*X(34)-JVS(181)*X(40)
  X(45) = X(45)-JVS(188)*X(34)-JVS(189)*X(40)
  X(46) = X(46)-JVS(193)*X(37)
  X(47) = X(47)-JVS(199)*X(34)-JVS(200)*X(40)
  X(49) = X(49)-JVS(211)*X(40)
  X(50) = X(50)-JVS(217)*X(34)-JVS(218)*X(40)-JVS(219)*X(41)-JVS(220)*X(42)-JVS(221)*X(43)
  X(51) = X(51)-JVS(229)*X(39)-JVS(230)*X(41)-JVS(231)*X(42)-JVS(232)*X(44)-JVS(233)*X(45)-JVS(234)*X(50)
  X(52) = X(52)-JVS(254)*X(28)-JVS(255)*X(43)-JVS(256)*X(46)-JVS(257)*X(47)-JVS(258)*X(49)-JVS(259)*X(50)
  X(54) = X(54)-JVS(280)*X(26)-JVS(281)*X(30)-JVS(282)*X(31)-JVS(283)*X(32)-JVS(284)*X(39)
  X(56) = X(56)-JVS(300)*X(34)-JVS(301)*X(40)-JVS(302)*X(41)-JVS(303)*X(42)-JVS(304)*X(44)-JVS(305)*X(45)-JVS(306)*X(49)&
            &-JVS(307)*X(53)
  X(57) = X(57)-JVS(318)*X(43)-JVS(319)*X(49)
  X(60) = X(60)-JVS(340)*X(30)-JVS(341)*X(32)-JVS(342)*X(39)-JVS(343)*X(41)-JVS(344)*X(42)-JVS(345)*X(54)-JVS(346)*X(58)&
            &-JVS(347)*X(59)
  X(62) = X(62)-JVS(375)*X(58)
  X(64) = X(64)-JVS(387)*X(58)
  X(66) = X(66)-JVS(399)*X(58)-JVS(400)*X(65)
  X(67) = X(67)-JVS(406)*X(19)-JVS(407)*X(25)-JVS(408)*X(30)-JVS(409)*X(32)-JVS(410)*X(39)-JVS(411)*X(53)-JVS(412)*X(55)&
            &-JVS(413)*X(63)-JVS(414)*X(64)-JVS(415)*X(65)
  X(68) = X(68)-JVS(430)*X(25)-JVS(431)*X(30)-JVS(432)*X(32)-JVS(433)*X(33)-JVS(434)*X(35)-JVS(435)*X(36)-JVS(436)*X(39)&
            &-JVS(437)*X(44)-JVS(438)*X(53)-JVS(439)*X(54)-JVS(440)*X(55)-JVS(441)*X(56)-JVS(442)*X(57)-JVS(443)*X(58)&
            &-JVS(444)*X(59)-JVS(445)*X(61)-JVS(446)*X(62)-JVS(447)*X(63)-JVS(448)*X(64)-JVS(449)*X(65)-JVS(450)*X(66)
  X(69) = X(69)-JVS(469)*X(31)-JVS(470)*X(59)-JVS(471)*X(61)-JVS(472)*X(63)-JVS(473)*X(64)-JVS(474)*X(65)
  X(70) = X(70)-JVS(483)*X(14)-JVS(484)*X(53)-JVS(485)*X(55)-JVS(486)*X(58)-JVS(487)*X(59)-JVS(488)*X(61)-JVS(489)*X(62)&
            &-JVS(490)*X(63)-JVS(491)*X(65)-JVS(492)*X(66)
  X(71) = X(71)-JVS(499)*X(26)-JVS(500)*X(30)-JVS(501)*X(32)-JVS(502)*X(39)-JVS(503)*X(41)-JVS(504)*X(42)-JVS(505)*X(45)&
            &-JVS(506)*X(48)-JVS(507)*X(53)-JVS(508)*X(55)-JVS(509)*X(59)-JVS(510)*X(61)-JVS(511)*X(62)-JVS(512)*X(63)&
            &-JVS(513)*X(64)-JVS(514)*X(65)-JVS(515)*X(66)-JVS(516)*X(69)-JVS(517)*X(70)
  X(72) = X(72)-JVS(529)*X(30)-JVS(530)*X(32)-JVS(531)*X(39)-JVS(532)*X(55)-JVS(533)*X(62)-JVS(534)*X(63)-JVS(535)*X(64)&
            &-JVS(536)*X(65)-JVS(537)*X(66)-JVS(538)*X(69)-JVS(539)*X(70)
  X(73) = X(73)-JVS(550)*X(32)-JVS(551)*X(39)-JVS(552)*X(40)-JVS(553)*X(58)-JVS(554)*X(59)-JVS(555)*X(61)-JVS(556)*X(63)&
            &-JVS(557)*X(64)-JVS(558)*X(65)-JVS(559)*X(66)-JVS(560)*X(69)-JVS(561)*X(70)
  X(74) = X(74)-JVS(574)*X(44)-JVS(575)*X(45)-JVS(576)*X(53)-JVS(577)*X(55)-JVS(578)*X(58)-JVS(579)*X(59)-JVS(580)*X(61)&
            &-JVS(581)*X(62)-JVS(582)*X(63)-JVS(583)*X(64)-JVS(584)*X(65)-JVS(585)*X(66)-JVS(586)*X(70)
  X(75) = X(75)-JVS(597)*X(23)-JVS(598)*X(58)-JVS(599)*X(62)-JVS(600)*X(64)-JVS(601)*X(65)-JVS(602)*X(66)-JVS(603)*X(70)&
            &-JVS(604)*X(74)
  X(76) = X(76)-JVS(617)*X(20)-JVS(618)*X(27)-JVS(619)*X(34)-JVS(620)*X(39)-JVS(621)*X(41)-JVS(622)*X(42)-JVS(623)*X(50)&
            &-JVS(624)*X(54)-JVS(625)*X(59)-JVS(626)*X(61)-JVS(627)*X(62)-JVS(628)*X(63)-JVS(629)*X(64)-JVS(630)*X(65)&
            &-JVS(631)*X(66)-JVS(632)*X(67)-JVS(633)*X(69)-JVS(634)*X(70)-JVS(635)*X(71)-JVS(636)*X(72)-JVS(637)*X(73)&
            &-JVS(638)*X(74)-JVS(639)*X(75)
  X(77) = X(77)-JVS(651)*X(19)-JVS(652)*X(25)-JVS(653)*X(26)-JVS(654)*X(30)-JVS(655)*X(32)-JVS(656)*X(34)-JVS(657)*X(39)&
            &-JVS(658)*X(40)-JVS(659)*X(41)-JVS(660)*X(42)-JVS(661)*X(43)-JVS(662)*X(44)-JVS(663)*X(45)-JVS(664)*X(48)&
            &-JVS(665)*X(49)-JVS(666)*X(53)-JVS(667)*X(55)-JVS(668)*X(57)-JVS(669)*X(58)-JVS(670)*X(59)-JVS(671)*X(61)&
            &-JVS(672)*X(62)-JVS(673)*X(63)-JVS(674)*X(64)-JVS(675)*X(65)-JVS(676)*X(66)-JVS(677)*X(69)-JVS(678)*X(70)&
            &-JVS(679)*X(71)-JVS(680)*X(72)-JVS(681)*X(73)-JVS(682)*X(74)-JVS(683)*X(75)-JVS(684)*X(76)
  X(78) = X(78)-JVS(695)*X(29)-JVS(696)*X(36)-JVS(697)*X(60)-JVS(698)*X(61)-JVS(699)*X(63)-JVS(700)*X(65)-JVS(701)*X(66)&
            &-JVS(702)*X(68)-JVS(703)*X(69)-JVS(704)*X(70)-JVS(705)*X(72)-JVS(706)*X(73)-JVS(707)*X(74)-JVS(708)*X(75)&
            &-JVS(709)*X(76)-JVS(710)*X(77)
  X(79) = X(79)-JVS(720)*X(20)-JVS(721)*X(21)-JVS(722)*X(22)-JVS(723)*X(23)-JVS(724)*X(28)-JVS(725)*X(29)-JVS(726)*X(31)&
            &-JVS(727)*X(36)-JVS(728)*X(37)-JVS(729)*X(38)-JVS(730)*X(46)-JVS(731)*X(52)-JVS(732)*X(56)-JVS(733)*X(57)&
            &-JVS(734)*X(58)-JVS(735)*X(59)-JVS(736)*X(60)-JVS(737)*X(61)-JVS(738)*X(62)-JVS(739)*X(63)-JVS(740)*X(64)&
            &-JVS(741)*X(65)-JVS(742)*X(66)-JVS(743)*X(67)-JVS(744)*X(68)-JVS(745)*X(69)-JVS(746)*X(70)-JVS(747)*X(71)&
            &-JVS(748)*X(72)-JVS(749)*X(73)-JVS(750)*X(74)-JVS(751)*X(75)-JVS(752)*X(76)-JVS(753)*X(77)-JVS(754)*X(78)
  X(80) = X(80)-JVS(763)*X(28)-JVS(764)*X(38)-JVS(765)*X(43)-JVS(766)*X(46)-JVS(767)*X(47)-JVS(768)*X(49)-JVS(769)*X(50)&
            &-JVS(770)*X(52)-JVS(771)*X(53)-JVS(772)*X(55)-JVS(773)*X(56)-JVS(774)*X(57)-JVS(775)*X(58)-JVS(776)*X(59)&
            &-JVS(777)*X(60)-JVS(778)*X(61)-JVS(779)*X(62)-JVS(780)*X(63)-JVS(781)*X(64)-JVS(782)*X(65)-JVS(783)*X(66)&
            &-JVS(784)*X(67)-JVS(785)*X(68)-JVS(786)*X(69)-JVS(787)*X(70)-JVS(788)*X(71)-JVS(789)*X(72)-JVS(790)*X(73)&
            &-JVS(791)*X(74)-JVS(792)*X(75)-JVS(793)*X(76)-JVS(794)*X(77)-JVS(795)*X(78)-JVS(796)*X(79)
  X(81) = X(81)-JVS(804)*X(15)-JVS(805)*X(31)-JVS(806)*X(35)-JVS(807)*X(39)-JVS(808)*X(53)-JVS(809)*X(54)-JVS(810)*X(55)&
            &-JVS(811)*X(58)-JVS(812)*X(59)-JVS(813)*X(61)-JVS(814)*X(63)-JVS(815)*X(65)-JVS(816)*X(66)-JVS(817)*X(67)&
            &-JVS(818)*X(69)-JVS(819)*X(70)-JVS(820)*X(71)-JVS(821)*X(72)-JVS(822)*X(73)-JVS(823)*X(74)-JVS(824)*X(75)&
            &-JVS(825)*X(76)-JVS(826)*X(77)-JVS(827)*X(78)-JVS(828)*X(79)-JVS(829)*X(80)
  X(82) = X(82)-JVS(836)*X(14)-JVS(837)*X(15)-JVS(838)*X(16)-JVS(839)*X(19)-JVS(840)*X(24)-JVS(841)*X(25)-JVS(842)*X(26)&
            &-JVS(843)*X(29)-JVS(844)*X(30)-JVS(845)*X(32)-JVS(846)*X(33)-JVS(847)*X(34)-JVS(848)*X(35)-JVS(849)*X(38)&
            &-JVS(850)*X(39)-JVS(851)*X(40)-JVS(852)*X(41)-JVS(853)*X(42)-JVS(854)*X(43)-JVS(855)*X(44)-JVS(856)*X(45)&
            &-JVS(857)*X(47)-JVS(858)*X(48)-JVS(859)*X(49)-JVS(860)*X(50)-JVS(861)*X(51)-JVS(862)*X(52)-JVS(863)*X(53)&
            &-JVS(864)*X(54)-JVS(865)*X(55)-JVS(866)*X(56)-JVS(867)*X(57)-JVS(868)*X(58)-JVS(869)*X(59)-JVS(870)*X(61)&
            &-JVS(871)*X(62)-JVS(872)*X(63)-JVS(873)*X(64)-JVS(874)*X(65)-JVS(875)*X(66)-JVS(876)*X(67)-JVS(877)*X(68)&
            &-JVS(878)*X(69)-JVS(879)*X(70)-JVS(880)*X(71)-JVS(881)*X(72)-JVS(882)*X(73)-JVS(883)*X(74)-JVS(884)*X(75)&
            &-JVS(885)*X(76)-JVS(886)*X(77)-JVS(887)*X(78)-JVS(888)*X(79)-JVS(889)*X(80)-JVS(890)*X(81)
  X(83) = X(83)-JVS(896)*X(21)-JVS(897)*X(56)-JVS(898)*X(57)-JVS(899)*X(59)-JVS(900)*X(61)-JVS(901)*X(62)-JVS(902)*X(64)&
            &-JVS(903)*X(65)-JVS(904)*X(66)-JVS(905)*X(70)-JVS(906)*X(71)-JVS(907)*X(72)-JVS(908)*X(73)-JVS(909)*X(74)&
            &-JVS(910)*X(75)-JVS(911)*X(76)-JVS(912)*X(77)-JVS(913)*X(78)-JVS(914)*X(79)-JVS(915)*X(80)-JVS(916)*X(81)&
            &-JVS(917)*X(82)
  X(84) = X(84)-JVS(922)*X(22)-JVS(923)*X(47)-JVS(924)*X(65)-JVS(925)*X(70)-JVS(926)*X(74)-JVS(927)*X(75)-JVS(928)*X(76)&
            &-JVS(929)*X(77)-JVS(930)*X(78)-JVS(931)*X(79)-JVS(932)*X(80)-JVS(933)*X(81)-JVS(934)*X(82)-JVS(935)*X(83)
  X(85) = X(85)-JVS(939)*X(16)-JVS(940)*X(24)-JVS(941)*X(25)-JVS(942)*X(29)-JVS(943)*X(33)-JVS(944)*X(34)-JVS(945)*X(35)&
            &-JVS(946)*X(36)-JVS(947)*X(37)-JVS(948)*X(38)-JVS(949)*X(40)-JVS(950)*X(41)-JVS(951)*X(42)-JVS(952)*X(44)&
            &-JVS(953)*X(45)-JVS(954)*X(46)-JVS(955)*X(48)-JVS(956)*X(50)-JVS(957)*X(51)-JVS(958)*X(53)-JVS(959)*X(55)&
            &-JVS(960)*X(56)-JVS(961)*X(57)-JVS(962)*X(58)-JVS(963)*X(59)-JVS(964)*X(61)-JVS(965)*X(62)-JVS(966)*X(63)&
            &-JVS(967)*X(64)-JVS(968)*X(65)-JVS(969)*X(66)-JVS(970)*X(67)-JVS(971)*X(68)-JVS(972)*X(69)-JVS(973)*X(70)&
            &-JVS(974)*X(71)-JVS(975)*X(72)-JVS(976)*X(73)-JVS(977)*X(74)-JVS(978)*X(75)-JVS(979)*X(76)-JVS(980)*X(77)&
            &-JVS(981)*X(78)-JVS(982)*X(79)-JVS(983)*X(80)-JVS(984)*X(81)-JVS(985)*X(82)-JVS(986)*X(83)-JVS(987)*X(84)
  X(86) = X(86)-JVS(990)*X(26)-JVS(991)*X(30)-JVS(992)*X(32)-JVS(993)*X(34)-JVS(994)*X(39)-JVS(995)*X(40)-JVS(996)*X(55)&
            &-JVS(997)*X(58)-JVS(998)*X(59)-JVS(999)*X(61)-JVS(1000)*X(63)-JVS(1001)*X(64)-JVS(1002)*X(65)-JVS(1003)*X(66)&
            &-JVS(1004)*X(69)-JVS(1005)*X(70)-JVS(1006)*X(71)-JVS(1007)*X(72)-JVS(1008)*X(73)-JVS(1009)*X(74)-JVS(1010)&
            &*X(75)-JVS(1011)*X(76)-JVS(1012)*X(77)-JVS(1013)*X(78)-JVS(1014)*X(79)-JVS(1015)*X(80)-JVS(1016)*X(81)&
            &-JVS(1017)*X(82)-JVS(1018)*X(83)-JVS(1019)*X(84)-JVS(1020)*X(85)
  X(86) = X(86)/JVS(1021)
  X(85) = (X(85)-JVS(989)*X(86))/(JVS(988))
  X(84) = (X(84)-JVS(937)*X(85)-JVS(938)*X(86))/(JVS(936))
  X(83) = (X(83)-JVS(919)*X(84)-JVS(920)*X(85)-JVS(921)*X(86))/(JVS(918))
  X(82) = (X(82)-JVS(892)*X(83)-JVS(893)*X(84)-JVS(894)*X(85)-JVS(895)*X(86))/(JVS(891))
  X(81) = (X(81)-JVS(831)*X(82)-JVS(832)*X(83)-JVS(833)*X(84)-JVS(834)*X(85)-JVS(835)*X(86))/(JVS(830))
  X(80) = (X(80)-JVS(798)*X(81)-JVS(799)*X(82)-JVS(800)*X(83)-JVS(801)*X(84)-JVS(802)*X(85)-JVS(803)*X(86))/(JVS(797))
  X(79) = (X(79)-JVS(756)*X(80)-JVS(757)*X(81)-JVS(758)*X(82)-JVS(759)*X(83)-JVS(760)*X(84)-JVS(761)*X(85)-JVS(762)&
            &*X(86))/(JVS(755))
  X(78) = (X(78)-JVS(712)*X(79)-JVS(713)*X(80)-JVS(714)*X(81)-JVS(715)*X(82)-JVS(716)*X(83)-JVS(717)*X(84)-JVS(718)&
            &*X(85)-JVS(719)*X(86))/(JVS(711))
  X(77) = (X(77)-JVS(686)*X(78)-JVS(687)*X(79)-JVS(688)*X(80)-JVS(689)*X(81)-JVS(690)*X(82)-JVS(691)*X(83)-JVS(692)&
            &*X(84)-JVS(693)*X(85)-JVS(694)*X(86))/(JVS(685))
  X(76) = (X(76)-JVS(641)*X(77)-JVS(642)*X(78)-JVS(643)*X(79)-JVS(644)*X(80)-JVS(645)*X(81)-JVS(646)*X(82)-JVS(647)&
            &*X(83)-JVS(648)*X(84)-JVS(649)*X(85)-JVS(650)*X(86))/(JVS(640))
  X(75) = (X(75)-JVS(606)*X(76)-JVS(607)*X(77)-JVS(608)*X(78)-JVS(609)*X(79)-JVS(610)*X(80)-JVS(611)*X(81)-JVS(612)&
            &*X(82)-JVS(613)*X(83)-JVS(614)*X(84)-JVS(615)*X(85)-JVS(616)*X(86))/(JVS(605))
  X(74) = (X(74)-JVS(588)*X(75)-JVS(589)*X(76)-JVS(590)*X(78)-JVS(591)*X(79)-JVS(592)*X(80)-JVS(593)*X(82)-JVS(594)&
            &*X(83)-JVS(595)*X(84)-JVS(596)*X(85))/(JVS(587))
  X(73) = (X(73)-JVS(563)*X(74)-JVS(564)*X(76)-JVS(565)*X(77)-JVS(566)*X(78)-JVS(567)*X(79)-JVS(568)*X(80)-JVS(569)&
            &*X(81)-JVS(570)*X(82)-JVS(571)*X(83)-JVS(572)*X(84)-JVS(573)*X(86))/(JVS(562))
  X(72) = (X(72)-JVS(541)*X(73)-JVS(542)*X(74)-JVS(543)*X(77)-JVS(544)*X(78)-JVS(545)*X(79)-JVS(546)*X(80)-JVS(547)&
            &*X(81)-JVS(548)*X(82)-JVS(549)*X(86))/(JVS(540))
  X(71) = (X(71)-JVS(519)*X(72)-JVS(520)*X(73)-JVS(521)*X(74)-JVS(522)*X(77)-JVS(523)*X(78)-JVS(524)*X(79)-JVS(525)&
            &*X(80)-JVS(526)*X(82)-JVS(527)*X(85)-JVS(528)*X(86))/(JVS(518))
  X(70) = (X(70)-JVS(494)*X(74)-JVS(495)*X(78)-JVS(496)*X(79)-JVS(497)*X(80)-JVS(498)*X(82))/(JVS(493))
  X(69) = (X(69)-JVS(476)*X(70)-JVS(477)*X(74)-JVS(478)*X(78)-JVS(479)*X(79)-JVS(480)*X(80)-JVS(481)*X(82)-JVS(482)&
            &*X(86))/(JVS(475))
  X(68) = (X(68)-JVS(452)*X(69)-JVS(453)*X(70)-JVS(454)*X(72)-JVS(455)*X(73)-JVS(456)*X(74)-JVS(457)*X(75)-JVS(458)&
            &*X(76)-JVS(459)*X(77)-JVS(460)*X(78)-JVS(461)*X(79)-JVS(462)*X(80)-JVS(463)*X(81)-JVS(464)*X(82)-JVS(465)*X(83)&
            &-JVS(466)*X(84)-JVS(467)*X(85)-JVS(468)*X(86))/(JVS(451))
  X(67) = (X(67)-JVS(417)*X(69)-JVS(418)*X(70)-JVS(419)*X(71)-JVS(420)*X(72)-JVS(421)*X(73)-JVS(422)*X(74)-JVS(423)&
            &*X(75)-JVS(424)*X(76)-JVS(425)*X(78)-JVS(426)*X(80)-JVS(427)*X(82)-JVS(428)*X(83)-JVS(429)*X(84))/(JVS(416))
  X(66) = (X(66)-JVS(402)*X(70)-JVS(403)*X(74)-JVS(404)*X(80)-JVS(405)*X(82))/(JVS(401))
  X(65) = (X(65)-JVS(395)*X(70)-JVS(396)*X(74)-JVS(397)*X(80)-JVS(398)*X(82))/(JVS(394))
  X(64) = (X(64)-JVS(389)*X(65)-JVS(390)*X(70)-JVS(391)*X(74)-JVS(392)*X(80)-JVS(393)*X(82))/(JVS(388))
  X(63) = (X(63)-JVS(383)*X(70)-JVS(384)*X(74)-JVS(385)*X(80)-JVS(386)*X(82))/(JVS(382))
  X(62) = (X(62)-JVS(377)*X(65)-JVS(378)*X(70)-JVS(379)*X(74)-JVS(380)*X(80)-JVS(381)*X(82))/(JVS(376))
  X(61) = (X(61)-JVS(371)*X(70)-JVS(372)*X(74)-JVS(373)*X(80)-JVS(374)*X(82))/(JVS(370))
  X(60) = (X(60)-JVS(349)*X(61)-JVS(350)*X(63)-JVS(351)*X(65)-JVS(352)*X(66)-JVS(353)*X(69)-JVS(354)*X(70)-JVS(355)&
            &*X(72)-JVS(356)*X(73)-JVS(357)*X(74)-JVS(358)*X(75)-JVS(359)*X(76)-JVS(360)*X(77)-JVS(361)*X(78)-JVS(362)*X(79)&
            &-JVS(363)*X(80)-JVS(364)*X(81)-JVS(365)*X(82)-JVS(366)*X(83)-JVS(367)*X(84)-JVS(368)*X(85)-JVS(369)*X(86))&
            &/(JVS(348))
  X(59) = (X(59)-JVS(336)*X(70)-JVS(337)*X(74)-JVS(338)*X(80)-JVS(339)*X(82))/(JVS(335))
  X(58) = (X(58)-JVS(331)*X(70)-JVS(332)*X(74)-JVS(333)*X(80)-JVS(334)*X(82))/(JVS(330))
  X(57) = (X(57)-JVS(321)*X(75)-JVS(322)*X(76)-JVS(323)*X(78)-JVS(324)*X(79)-JVS(325)*X(80)-JVS(326)*X(82)-JVS(327)&
            &*X(83)-JVS(328)*X(84)-JVS(329)*X(85))/(JVS(320))
  X(56) = (X(56)-JVS(309)*X(57)-JVS(310)*X(59)-JVS(311)*X(61)-JVS(312)*X(64)-JVS(313)*X(70)-JVS(314)*X(74)-JVS(315)&
            &*X(80)-JVS(316)*X(82)-JVS(317)*X(85))/(JVS(308))
  X(55) = (X(55)-JVS(296)*X(70)-JVS(297)*X(74)-JVS(298)*X(80)-JVS(299)*X(82))/(JVS(295))
  X(54) = (X(54)-JVS(286)*X(59)-JVS(287)*X(61)-JVS(288)*X(63)-JVS(289)*X(65)-JVS(290)*X(69)-JVS(291)*X(74)-JVS(292)&
            &*X(79)-JVS(293)*X(80)-JVS(294)*X(82))/(JVS(285))
  X(53) = (X(53)-JVS(276)*X(70)-JVS(277)*X(74)-JVS(278)*X(80)-JVS(279)*X(82))/(JVS(275))
  X(52) = (X(52)-JVS(261)*X(56)-JVS(262)*X(57)-JVS(263)*X(62)-JVS(264)*X(64)-JVS(265)*X(65)-JVS(266)*X(66)-JVS(267)&
            &*X(67)-JVS(268)*X(68)-JVS(269)*X(71)-JVS(270)*X(74)-JVS(271)*X(79)-JVS(272)*X(80)-JVS(273)*X(82)-JVS(274)&
            &*X(85))/(JVS(260))
  X(51) = (X(51)-JVS(236)*X(53)-JVS(237)*X(55)-JVS(238)*X(56)-JVS(239)*X(58)-JVS(240)*X(59)-JVS(241)*X(61)-JVS(242)&
            &*X(62)-JVS(243)*X(63)-JVS(244)*X(64)-JVS(245)*X(65)-JVS(246)*X(66)-JVS(247)*X(67)-JVS(248)*X(68)-JVS(249)*X(70)&
            &-JVS(250)*X(71)-JVS(251)*X(74)-JVS(252)*X(80)-JVS(253)*X(82))/(JVS(235))
  X(50) = (X(50)-JVS(223)*X(62)-JVS(224)*X(64)-JVS(225)*X(66)-JVS(226)*X(74)-JVS(227)*X(80)-JVS(228)*X(82))/(JVS(222))
  X(49) = (X(49)-JVS(213)*X(57)-JVS(214)*X(80)-JVS(215)*X(82)-JVS(216)*X(85))/(JVS(212))
  X(48) = (X(48)-JVS(207)*X(77)-JVS(208)*X(82)-JVS(209)*X(85)-JVS(210)*X(86))/(JVS(206))
  X(47) = (X(47)-JVS(202)*X(65)-JVS(203)*X(74)-JVS(204)*X(80)-JVS(205)*X(82))/(JVS(201))
  X(46) = (X(46)-JVS(195)*X(57)-JVS(196)*X(79)-JVS(197)*X(80)-JVS(198)*X(85))/(JVS(194))
  X(45) = (X(45)-JVS(191)*X(74)-JVS(192)*X(82))/(JVS(190))
  X(44) = (X(44)-JVS(186)*X(74)-JVS(187)*X(82))/(JVS(185))
  X(43) = (X(43)-JVS(183)*X(80)-JVS(184)*X(82))/(JVS(182))
  X(42) = (X(42)-JVS(179)*X(82))/(JVS(178))
  X(41) = (X(41)-JVS(175)*X(82))/(JVS(174))
  X(40) = (X(40)-JVS(171)*X(82))/(JVS(170))
  X(39) = (X(39)-JVS(169)*X(82))/(JVS(168))
  X(38) = (X(38)-JVS(165)*X(79)-JVS(166)*X(82)-JVS(167)*X(85))/(JVS(164))
  X(37) = (X(37)-JVS(160)*X(46)-JVS(161)*X(79)-JVS(162)*X(80)-JVS(163)*X(85))/(JVS(159))
  X(36) = (X(36)-JVS(156)*X(68)-JVS(157)*X(78)-JVS(158)*X(85))/(JVS(155))
  X(35) = (X(35)-JVS(152)*X(81)-JVS(153)*X(82)-JVS(154)*X(85))/(JVS(151))
  X(34) = (X(34)-JVS(150)*X(82))/(JVS(149))
  X(33) = (X(33)-JVS(145)*X(77)-JVS(146)*X(81)-JVS(147)*X(82)-JVS(148)*X(86))/(JVS(144))
  X(32) = (X(32)-JVS(143)*X(82))/(JVS(142))
  X(31) = (X(31)-JVS(140)*X(79)-JVS(141)*X(82))/(JVS(139))
  X(30) = (X(30)-JVS(137)*X(82))/(JVS(136))
  X(29) = (X(29)-JVS(134)*X(78)-JVS(135)*X(82))/(JVS(133))
  X(28) = (X(28)-JVS(131)*X(79)-JVS(132)*X(80))/(JVS(130))
  X(27) = (X(27)-JVS(125)*X(34)-JVS(126)*X(59)-JVS(127)*X(61)-JVS(128)*X(74)-JVS(129)*X(82))/(JVS(124))
  X(26) = (X(26)-JVS(123)*X(82))/(JVS(122))
  X(25) = (X(25)-JVS(121)*X(82))/(JVS(120))
  X(24) = (X(24)-JVS(118)*X(82)-JVS(119)*X(85))/(JVS(117))
  X(23) = (X(23)-JVS(115)*X(75)-JVS(116)*X(79))/(JVS(114))
  X(22) = (X(22)-JVS(112)*X(79)-JVS(113)*X(84))/(JVS(111))
  X(21) = (X(21)-JVS(109)*X(79)-JVS(110)*X(83))/(JVS(108))
  X(20) = (X(20)-JVS(106)*X(76)-JVS(107)*X(79))/(JVS(105))
  X(19) = (X(19)-JVS(104)*X(82))/(JVS(103))
  X(18) = (X(18)-JVS(99)*X(58)-JVS(100)*X(78)-JVS(101)*X(82)-JVS(102)*X(85))/(JVS(98))
  X(17) = (X(17)-JVS(94)*X(18)-JVS(95)*X(82)-JVS(96)*X(85))/(JVS(93))
  X(16) = (X(16)-JVS(92)*X(82))/(JVS(91))
  X(15) = (X(15)-JVS(90)*X(82))/(JVS(89))
  X(14) = (X(14)-JVS(88)*X(74))/(JVS(87))
  X(13) = (X(13)-JVS(79)*X(37)-JVS(80)*X(47)-JVS(81)*X(55)-JVS(82)*X(70)-JVS(83)*X(74)-JVS(84)*X(79)-JVS(85)*X(80)&
            &-JVS(86)*X(82))/(JVS(78))
  X(12) = (X(12)-JVS(74)*X(37)-JVS(75)*X(55)-JVS(76)*X(79)-JVS(77)*X(80))/(JVS(73))
  X(11) = (X(11)-JVS(69)*X(75)-JVS(70)*X(83)-JVS(71)*X(84)-JVS(72)*X(85))/(JVS(68))
  X(10) = (X(10)-JVS(66)*X(76)-JVS(67)*X(85))/(JVS(65))
  X(9) = (X(9)-JVS(62)*X(44)-JVS(63)*X(55)-JVS(64)*X(74))/(JVS(61))
  X(8) = (X(8)-JVS(57)*X(17)-JVS(58)*X(78)-JVS(59)*X(82)-JVS(60)*X(85))/(JVS(56))
  X(7) = (X(7)-JVS(54)*X(17)-JVS(55)*X(82))/(JVS(53))
  X(6) = (X(6)-JVS(49)*X(7)-JVS(50)*X(8)-JVS(51)*X(18)-JVS(52)*X(82))/(JVS(48))
  X(5) = (X(5)-JVS(46)*X(8)-JVS(47)*X(85))/(JVS(45))
  X(4) = (X(4)-JVS(31)*X(58)-JVS(32)*X(59)-JVS(33)*X(61)-JVS(34)*X(63)-JVS(35)*X(64)-JVS(36)*X(65)-JVS(37)*X(74)-JVS(38)&
           &*X(75)-JVS(39)*X(77)-JVS(40)*X(81)-JVS(41)*X(83)-JVS(42)*X(84)-JVS(43)*X(85)-JVS(44)*X(86))/(JVS(30))
  X(3) = (X(3)-JVS(21)*X(55)-JVS(22)*X(63)-JVS(23)*X(65)-JVS(24)*X(74)-JVS(25)*X(76)-JVS(26)*X(77)-JVS(27)*X(81)-JVS(28)&
           &*X(85)-JVS(29)*X(86))/(JVS(20))
  X(2) = (X(2)-JVS(5)*X(36)-JVS(6)*X(44)-JVS(7)*X(53)-JVS(8)*X(55)-JVS(9)*X(58)-JVS(10)*X(59)-JVS(11)*X(61)-JVS(12)&
           &*X(62)-JVS(13)*X(63)-JVS(14)*X(64)-JVS(15)*X(65)-JVS(16)*X(66)-JVS(17)*X(74)-JVS(18)*X(78)-JVS(19)*X(82))&
           &/(JVS(4))
  X(1) = (X(1)-JVS(2)*X(16)-JVS(3)*X(82))/(JVS(1))
      
END SUBROUTINE saprc99_KppSolve
























      SUBROUTINE saprc99_WCOPY(N,X,incX,Y,incY)








      
      INTEGER i,incX,incY,M,MP1,N
      REAL(kind=dp) X(N),Y(N)

      IF (N.LE.0) RETURN

      M = MOD(N,8)
      IF( M .NE. 0 ) THEN
        DO i = 1,M
          Y(i) = X(i)
        END DO
        IF( N .LT. 8 ) RETURN
      END IF    
      MP1 = M+1
      DO i = MP1,N,8
        Y(i) = X(i)
        Y(i + 1) = X(i + 1)
        Y(i + 2) = X(i + 2)
        Y(i + 3) = X(i + 3)
        Y(i + 4) = X(i + 4)
        Y(i + 5) = X(i + 5)
        Y(i + 6) = X(i + 6)
        Y(i + 7) = X(i + 7)
      END DO

      END SUBROUTINE saprc99_WCOPY



      SUBROUTINE saprc99_WAXPY(N,Alpha,X,incX,Y,incY)









      INTEGER i,incX,incY,M,MP1,N
      REAL(kind=dp) X(N),Y(N),Alpha
      REAL(kind=dp) ZERO
      PARAMETER( ZERO = 0.0_dp )

      IF (Alpha .EQ. ZERO) RETURN
      IF (N .LE. 0) RETURN

      M = MOD(N,4)
      IF( M .NE. 0 ) THEN
        DO i = 1,M
          Y(i) = Y(i) + Alpha*X(i)
        END DO
        IF( N .LT. 4 ) RETURN
      END IF
      MP1 = M + 1
      DO i = MP1,N,4
        Y(i) = Y(i) + Alpha*X(i)
        Y(i + 1) = Y(i + 1) + Alpha*X(i + 1)
        Y(i + 2) = Y(i + 2) + Alpha*X(i + 2)
        Y(i + 3) = Y(i + 3) + Alpha*X(i + 3)
      END DO
      
      END SUBROUTINE saprc99_WAXPY




      SUBROUTINE saprc99_WSCAL(N,Alpha,X,incX)









      INTEGER i,incX,M,MP1,N
      REAL(kind=dp) X(N),Alpha
      REAL(kind=dp) ZERO, ONE
      PARAMETER( ZERO = 0.0_dp ) 
      PARAMETER( ONE  = 1.0_dp )

      IF (Alpha .EQ. ONE) RETURN
      IF (N .LE. 0) RETURN

      M = MOD(N,5)
      IF( M .NE. 0 ) THEN
        IF (Alpha .EQ. (-ONE)) THEN
          DO i = 1,M
            X(i) = -X(i)
          END DO
        ELSEIF (Alpha .EQ. ZERO) THEN
          DO i = 1,M
            X(i) = ZERO
          END DO
        ELSE
          DO i = 1,M
            X(i) = Alpha*X(i)
          END DO
        END IF
        IF( N .LT. 5 ) RETURN
      END IF
      MP1 = M + 1
      IF (Alpha .EQ. (-ONE)) THEN
        DO i = MP1,N,5
          X(i)     = -X(i)
          X(i + 1) = -X(i + 1)
          X(i + 2) = -X(i + 2)
          X(i + 3) = -X(i + 3)
          X(i + 4) = -X(i + 4)
        END DO
      ELSEIF (Alpha .EQ. ZERO) THEN
        DO i = MP1,N,5
          X(i)     = ZERO
          X(i + 1) = ZERO
          X(i + 2) = ZERO
          X(i + 3) = ZERO
          X(i + 4) = ZERO
        END DO
      ELSE
        DO i = MP1,N,5
          X(i)     = Alpha*X(i)
          X(i + 1) = Alpha*X(i + 1)
          X(i + 2) = Alpha*X(i + 2)
          X(i + 3) = Alpha*X(i + 3)
          X(i + 4) = Alpha*X(i + 4)
        END DO
      END IF

      END SUBROUTINE saprc99_WSCAL


      REAL(kind=dp) FUNCTION saprc99_WLAMCH( C )








      CHARACTER C
      INTEGER   i
      REAL(kind=dp)  ONE, HALF, Eps, Sum
      PARAMETER (ONE  = 1.0_dp)
      PARAMETER (HALF = 0.5_dp)
      LOGICAL   First
      SAVE     First, Eps
      DATA     First /.TRUE./
      
      IF (First) THEN
        First = .FALSE.
        Eps = HALF**(16)
        DO i = 17, 80
          Eps = Eps*HALF
          CALL saprc99_WLAMCH_ADD(ONE,Eps,Sum)
          IF (Sum.LE.ONE) GOTO 10
        END DO
        PRINT*,'ERROR IN WLAMCH. EPS < ',Eps
        RETURN
10      Eps = Eps*2
        i = i-1      
      END IF

      saprc99_WLAMCH = Eps

      END FUNCTION saprc99_WLAMCH
     
      SUBROUTINE saprc99_WLAMCH_ADD( A, B, Sum )

      
      REAL(kind=dp) A, B, Sum
      Sum = A + B

      END SUBROUTINE saprc99_WLAMCH_ADD




      SUBROUTINE saprc99_SET2ZERO(N,Y)




      
      INTEGER ::  i,M,MP1,N
      REAL(kind=dp) ::  Y(N)
      REAL(kind=dp), PARAMETER :: ZERO = 0.0d0

      IF (N.LE.0) RETURN

      M = MOD(N,8)
      IF( M .NE. 0 ) THEN
        DO i = 1,M
          Y(i) = ZERO
        END DO
        IF( N .LT. 8 ) RETURN
      END IF    
      MP1 = M+1
      DO i = MP1,N,8
        Y(i)     = ZERO
        Y(i + 1) = ZERO
        Y(i + 2) = ZERO
        Y(i + 3) = ZERO
        Y(i + 4) = ZERO
        Y(i + 5) = ZERO
        Y(i + 6) = ZERO
        Y(i + 7) = ZERO
      END DO

      END SUBROUTINE saprc99_SET2ZERO



      REAL(kind=dp) FUNCTION saprc99_WDOT (N, DX, incX, DY, incY) 









      IMPLICIT NONE
      INTEGER :: N, incX, incY
      REAL(kind=dp) :: DX(N), DY(N) 

      INTEGER :: i, IX, IY, M, MP1, NS
                                 
      saprc99_WDOT = 0.0D0 
      IF (N .LE. 0) RETURN 
      IF (incX .EQ. incY) IF (incX-1) 5,20,60 



    5 IX = 1 
      IY = 1 
      IF (incX .LT. 0) IX = (-N+1)*incX + 1 
      IF (incY .LT. 0) IY = (-N+1)*incY + 1 
      DO i = 1,N 
        saprc99_WDOT = saprc99_WDOT + DX(IX)*DY(IY) 
        IX = IX + incX 
        IY = IY + incY 
      END DO 
      RETURN 





   20 M = MOD(N,5) 
      IF (M .EQ. 0) GO TO 40 
      DO i = 1,M 
         saprc99_WDOT = saprc99_WDOT + DX(i)*DY(i) 
      END DO 
      IF (N .LT. 5) RETURN 
   40 MP1 = M + 1 
      DO i = MP1,N,5 
          saprc99_WDOT = saprc99_WDOT + DX(i)*DY(i) + DX(i+1)*DY(i+1) +&
                   DX(i+2)*DY(i+2) +  &
                   DX(i+3)*DY(i+3) + DX(i+4)*DY(i+4)                   
      END DO 
      RETURN 



   60 NS = N*incX 
      DO i = 1,NS,incX 
        saprc99_WDOT = saprc99_WDOT + DX(i)*DY(i) 
      END DO 

      END FUNCTION saprc99_WDOT                                          




   SUBROUTINE decomp_saprc99( JVS, IER )
   
     IMPLICIT NONE
   
      INTEGER  :: IER
      REAL(kind=dp) :: JVS(LU_NONZERO), W(NVAR), a
   
   
  a = 0._dp
  ier = 0 
   
  IF ( ABS(  JVS( 1 )) < TINY(a) ) THEN
         IER = 1                                       
         RETURN
  END IF
   W( 1 ) = JVS( 1 )
   W( 16 ) = JVS( 2 )
   W( 82 ) = JVS( 3 )
  JVS( 1) = W( 1 )
  JVS( 2) = W( 16 )
  JVS( 3) = W( 82 )
  IF ( ABS(  JVS( 4 )) < TINY(a) ) THEN
         IER = 2                                       
         RETURN
  END IF
   W( 2 ) = JVS( 4 )
   W( 36 ) = JVS( 5 )
   W( 44 ) = JVS( 6 )
   W( 53 ) = JVS( 7 )
   W( 55 ) = JVS( 8 )
   W( 58 ) = JVS( 9 )
   W( 59 ) = JVS( 10 )
   W( 61 ) = JVS( 11 )
   W( 62 ) = JVS( 12 )
   W( 63 ) = JVS( 13 )
   W( 64 ) = JVS( 14 )
   W( 65 ) = JVS( 15 )
   W( 66 ) = JVS( 16 )
   W( 74 ) = JVS( 17 )
   W( 78 ) = JVS( 18 )
   W( 82 ) = JVS( 19 )
  JVS( 4) = W( 2 )
  JVS( 5) = W( 36 )
  JVS( 6) = W( 44 )
  JVS( 7) = W( 53 )
  JVS( 8) = W( 55 )
  JVS( 9) = W( 58 )
  JVS( 10) = W( 59 )
  JVS( 11) = W( 61 )
  JVS( 12) = W( 62 )
  JVS( 13) = W( 63 )
  JVS( 14) = W( 64 )
  JVS( 15) = W( 65 )
  JVS( 16) = W( 66 )
  JVS( 17) = W( 74 )
  JVS( 18) = W( 78 )
  JVS( 19) = W( 82 )
  IF ( ABS(  JVS( 20 )) < TINY(a) ) THEN
         IER = 3                                       
         RETURN
  END IF
   W( 3 ) = JVS( 20 )
   W( 55 ) = JVS( 21 )
   W( 63 ) = JVS( 22 )
   W( 65 ) = JVS( 23 )
   W( 74 ) = JVS( 24 )
   W( 76 ) = JVS( 25 )
   W( 77 ) = JVS( 26 )
   W( 81 ) = JVS( 27 )
   W( 85 ) = JVS( 28 )
   W( 86 ) = JVS( 29 )
  JVS( 20) = W( 3 )
  JVS( 21) = W( 55 )
  JVS( 22) = W( 63 )
  JVS( 23) = W( 65 )
  JVS( 24) = W( 74 )
  JVS( 25) = W( 76 )
  JVS( 26) = W( 77 )
  JVS( 27) = W( 81 )
  JVS( 28) = W( 85 )
  JVS( 29) = W( 86 )
  IF ( ABS(  JVS( 30 )) < TINY(a) ) THEN
         IER = 4                                       
         RETURN
  END IF
   W( 4 ) = JVS( 30 )
   W( 58 ) = JVS( 31 )
   W( 59 ) = JVS( 32 )
   W( 61 ) = JVS( 33 )
   W( 63 ) = JVS( 34 )
   W( 64 ) = JVS( 35 )
   W( 65 ) = JVS( 36 )
   W( 74 ) = JVS( 37 )
   W( 75 ) = JVS( 38 )
   W( 77 ) = JVS( 39 )
   W( 81 ) = JVS( 40 )
   W( 83 ) = JVS( 41 )
   W( 84 ) = JVS( 42 )
   W( 85 ) = JVS( 43 )
   W( 86 ) = JVS( 44 )
  JVS( 30) = W( 4 )
  JVS( 31) = W( 58 )
  JVS( 32) = W( 59 )
  JVS( 33) = W( 61 )
  JVS( 34) = W( 63 )
  JVS( 35) = W( 64 )
  JVS( 36) = W( 65 )
  JVS( 37) = W( 74 )
  JVS( 38) = W( 75 )
  JVS( 39) = W( 77 )
  JVS( 40) = W( 81 )
  JVS( 41) = W( 83 )
  JVS( 42) = W( 84 )
  JVS( 43) = W( 85 )
  JVS( 44) = W( 86 )
  IF ( ABS(  JVS( 45 )) < TINY(a) ) THEN
         IER = 5                                       
         RETURN
  END IF
   W( 5 ) = JVS( 45 )
   W( 8 ) = JVS( 46 )
   W( 85 ) = JVS( 47 )
  JVS( 45) = W( 5 )
  JVS( 46) = W( 8 )
  JVS( 47) = W( 85 )
  IF ( ABS(  JVS( 48 )) < TINY(a) ) THEN
         IER = 6                                       
         RETURN
  END IF
   W( 6 ) = JVS( 48 )
   W( 7 ) = JVS( 49 )
   W( 8 ) = JVS( 50 )
   W( 18 ) = JVS( 51 )
   W( 82 ) = JVS( 52 )
  JVS( 48) = W( 6 )
  JVS( 49) = W( 7 )
  JVS( 50) = W( 8 )
  JVS( 51) = W( 18 )
  JVS( 52) = W( 82 )
  IF ( ABS(  JVS( 53 )) < TINY(a) ) THEN
         IER = 7                                       
         RETURN
  END IF
   W( 7 ) = JVS( 53 )
   W( 17 ) = JVS( 54 )
   W( 82 ) = JVS( 55 )
  JVS( 53) = W( 7 )
  JVS( 54) = W( 17 )
  JVS( 55) = W( 82 )
  IF ( ABS(  JVS( 56 )) < TINY(a) ) THEN
         IER = 8                                       
         RETURN
  END IF
   W( 8 ) = JVS( 56 )
   W( 17 ) = JVS( 57 )
   W( 78 ) = JVS( 58 )
   W( 82 ) = JVS( 59 )
   W( 85 ) = JVS( 60 )
  JVS( 56) = W( 8 )
  JVS( 57) = W( 17 )
  JVS( 58) = W( 78 )
  JVS( 59) = W( 82 )
  JVS( 60) = W( 85 )
  IF ( ABS(  JVS( 61 )) < TINY(a) ) THEN
         IER = 9                                       
         RETURN
  END IF
   W( 9 ) = JVS( 61 )
   W( 44 ) = JVS( 62 )
   W( 55 ) = JVS( 63 )
   W( 74 ) = JVS( 64 )
  JVS( 61) = W( 9 )
  JVS( 62) = W( 44 )
  JVS( 63) = W( 55 )
  JVS( 64) = W( 74 )
  IF ( ABS(  JVS( 65 )) < TINY(a) ) THEN
         IER = 10                                      
         RETURN
  END IF
   W( 10 ) = JVS( 65 )
   W( 76 ) = JVS( 66 )
   W( 85 ) = JVS( 67 )
  JVS( 65) = W( 10 )
  JVS( 66) = W( 76 )
  JVS( 67) = W( 85 )
  IF ( ABS(  JVS( 68 )) < TINY(a) ) THEN
         IER = 11                                      
         RETURN
  END IF
   W( 11 ) = JVS( 68 )
   W( 75 ) = JVS( 69 )
   W( 83 ) = JVS( 70 )
   W( 84 ) = JVS( 71 )
   W( 85 ) = JVS( 72 )
  JVS( 68) = W( 11 )
  JVS( 69) = W( 75 )
  JVS( 70) = W( 83 )
  JVS( 71) = W( 84 )
  JVS( 72) = W( 85 )
  IF ( ABS(  JVS( 73 )) < TINY(a) ) THEN
         IER = 12                                      
         RETURN
  END IF
   W( 12 ) = JVS( 73 )
   W( 37 ) = JVS( 74 )
   W( 55 ) = JVS( 75 )
   W( 79 ) = JVS( 76 )
   W( 80 ) = JVS( 77 )
  JVS( 73) = W( 12 )
  JVS( 74) = W( 37 )
  JVS( 75) = W( 55 )
  JVS( 76) = W( 79 )
  JVS( 77) = W( 80 )
  IF ( ABS(  JVS( 78 )) < TINY(a) ) THEN
         IER = 13                                      
         RETURN
  END IF
   W( 13 ) = JVS( 78 )
   W( 37 ) = JVS( 79 )
   W( 47 ) = JVS( 80 )
   W( 55 ) = JVS( 81 )
   W( 70 ) = JVS( 82 )
   W( 74 ) = JVS( 83 )
   W( 79 ) = JVS( 84 )
   W( 80 ) = JVS( 85 )
   W( 82 ) = JVS( 86 )
  JVS( 78) = W( 13 )
  JVS( 79) = W( 37 )
  JVS( 80) = W( 47 )
  JVS( 81) = W( 55 )
  JVS( 82) = W( 70 )
  JVS( 83) = W( 74 )
  JVS( 84) = W( 79 )
  JVS( 85) = W( 80 )
  JVS( 86) = W( 82 )
  IF ( ABS(  JVS( 87 )) < TINY(a) ) THEN
         IER = 14                                      
         RETURN
  END IF
   W( 14 ) = JVS( 87 )
   W( 74 ) = JVS( 88 )
  JVS( 87) = W( 14 )
  JVS( 88) = W( 74 )
  IF ( ABS(  JVS( 89 )) < TINY(a) ) THEN
         IER = 15                                      
         RETURN
  END IF
   W( 15 ) = JVS( 89 )
   W( 82 ) = JVS( 90 )
  JVS( 89) = W( 15 )
  JVS( 90) = W( 82 )
  IF ( ABS(  JVS( 91 )) < TINY(a) ) THEN
         IER = 16                                      
         RETURN
  END IF
   W( 16 ) = JVS( 91 )
   W( 82 ) = JVS( 92 )
  JVS( 91) = W( 16 )
  JVS( 92) = W( 82 )
  IF ( ABS(  JVS( 93 )) < TINY(a) ) THEN
         IER = 17                                      
         RETURN
  END IF
   W( 17 ) = JVS( 93 )
   W( 18 ) = JVS( 94 )
   W( 82 ) = JVS( 95 )
   W( 85 ) = JVS( 96 )
  JVS( 93) = W( 17 )
  JVS( 94) = W( 18 )
  JVS( 95) = W( 82 )
  JVS( 96) = W( 85 )
  IF ( ABS(  JVS( 98 )) < TINY(a) ) THEN
         IER = 18                                      
         RETURN
  END IF
   W( 17 ) = JVS( 97 )
   W( 18 ) = JVS( 98 )
   W( 58 ) = JVS( 99 )
   W( 78 ) = JVS( 100 )
   W( 82 ) = JVS( 101 )
   W( 85 ) = JVS( 102 )
  a = -W( 17 ) / JVS(           93  )
  W( 17 ) = -a
  W( 18 ) = W( 18 ) + a*JVS( 94 )
  W( 82 ) = W( 82 ) + a*JVS( 95 )
  W( 85 ) = W( 85 ) + a*JVS( 96 )
  JVS( 97) = W( 17 )
  JVS( 98) = W( 18 )
  JVS( 99) = W( 58 )
  JVS( 100) = W( 78 )
  JVS( 101) = W( 82 )
  JVS( 102) = W( 85 )
  IF ( ABS(  JVS( 103 )) < TINY(a) ) THEN
         IER = 19                                      
         RETURN
  END IF
   W( 19 ) = JVS( 103 )
   W( 82 ) = JVS( 104 )
  JVS( 103) = W( 19 )
  JVS( 104) = W( 82 )
  IF ( ABS(  JVS( 105 )) < TINY(a) ) THEN
         IER = 20                                      
         RETURN
  END IF
   W( 20 ) = JVS( 105 )
   W( 76 ) = JVS( 106 )
   W( 79 ) = JVS( 107 )
  JVS( 105) = W( 20 )
  JVS( 106) = W( 76 )
  JVS( 107) = W( 79 )
  IF ( ABS(  JVS( 108 )) < TINY(a) ) THEN
         IER = 21                                      
         RETURN
  END IF
   W( 21 ) = JVS( 108 )
   W( 79 ) = JVS( 109 )
   W( 83 ) = JVS( 110 )
  JVS( 108) = W( 21 )
  JVS( 109) = W( 79 )
  JVS( 110) = W( 83 )
  IF ( ABS(  JVS( 111 )) < TINY(a) ) THEN
         IER = 22                                      
         RETURN
  END IF
   W( 22 ) = JVS( 111 )
   W( 79 ) = JVS( 112 )
   W( 84 ) = JVS( 113 )
  JVS( 111) = W( 22 )
  JVS( 112) = W( 79 )
  JVS( 113) = W( 84 )
  IF ( ABS(  JVS( 114 )) < TINY(a) ) THEN
         IER = 23                                      
         RETURN
  END IF
   W( 23 ) = JVS( 114 )
   W( 75 ) = JVS( 115 )
   W( 79 ) = JVS( 116 )
  JVS( 114) = W( 23 )
  JVS( 115) = W( 75 )
  JVS( 116) = W( 79 )
  IF ( ABS(  JVS( 117 )) < TINY(a) ) THEN
         IER = 24                                      
         RETURN
  END IF
   W( 24 ) = JVS( 117 )
   W( 82 ) = JVS( 118 )
   W( 85 ) = JVS( 119 )
  JVS( 117) = W( 24 )
  JVS( 118) = W( 82 )
  JVS( 119) = W( 85 )
  IF ( ABS(  JVS( 120 )) < TINY(a) ) THEN
         IER = 25                                      
         RETURN
  END IF
   W( 25 ) = JVS( 120 )
   W( 82 ) = JVS( 121 )
  JVS( 120) = W( 25 )
  JVS( 121) = W( 82 )
  IF ( ABS(  JVS( 122 )) < TINY(a) ) THEN
         IER = 26                                      
         RETURN
  END IF
   W( 26 ) = JVS( 122 )
   W( 82 ) = JVS( 123 )
  JVS( 122) = W( 26 )
  JVS( 123) = W( 82 )
  IF ( ABS(  JVS( 124 )) < TINY(a) ) THEN
         IER = 27                                      
         RETURN
  END IF
   W( 27 ) = JVS( 124 )
   W( 34 ) = JVS( 125 )
   W( 59 ) = JVS( 126 )
   W( 61 ) = JVS( 127 )
   W( 74 ) = JVS( 128 )
   W( 82 ) = JVS( 129 )
  JVS( 124) = W( 27 )
  JVS( 125) = W( 34 )
  JVS( 126) = W( 59 )
  JVS( 127) = W( 61 )
  JVS( 128) = W( 74 )
  JVS( 129) = W( 82 )
  IF ( ABS(  JVS( 130 )) < TINY(a) ) THEN
         IER = 28                                      
         RETURN
  END IF
   W( 28 ) = JVS( 130 )
   W( 79 ) = JVS( 131 )
   W( 80 ) = JVS( 132 )
  JVS( 130) = W( 28 )
  JVS( 131) = W( 79 )
  JVS( 132) = W( 80 )
  IF ( ABS(  JVS( 133 )) < TINY(a) ) THEN
         IER = 29                                      
         RETURN
  END IF
   W( 29 ) = JVS( 133 )
   W( 78 ) = JVS( 134 )
   W( 82 ) = JVS( 135 )
  JVS( 133) = W( 29 )
  JVS( 134) = W( 78 )
  JVS( 135) = W( 82 )
  IF ( ABS(  JVS( 136 )) < TINY(a) ) THEN
         IER = 30                                      
         RETURN
  END IF
   W( 30 ) = JVS( 136 )
   W( 82 ) = JVS( 137 )
  JVS( 136) = W( 30 )
  JVS( 137) = W( 82 )
  IF ( ABS(  JVS( 139 )) < TINY(a) ) THEN
         IER = 31                                      
         RETURN
  END IF
   W( 30 ) = JVS( 138 )
   W( 31 ) = JVS( 139 )
   W( 79 ) = JVS( 140 )
   W( 82 ) = JVS( 141 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  JVS( 138) = W( 30 )
  JVS( 139) = W( 31 )
  JVS( 140) = W( 79 )
  JVS( 141) = W( 82 )
  IF ( ABS(  JVS( 142 )) < TINY(a) ) THEN
         IER = 32                                      
         RETURN
  END IF
   W( 32 ) = JVS( 142 )
   W( 82 ) = JVS( 143 )
  JVS( 142) = W( 32 )
  JVS( 143) = W( 82 )
  IF ( ABS(  JVS( 144 )) < TINY(a) ) THEN
         IER = 33                                      
         RETURN
  END IF
   W( 33 ) = JVS( 144 )
   W( 77 ) = JVS( 145 )
   W( 81 ) = JVS( 146 )
   W( 82 ) = JVS( 147 )
   W( 86 ) = JVS( 148 )
  JVS( 144) = W( 33 )
  JVS( 145) = W( 77 )
  JVS( 146) = W( 81 )
  JVS( 147) = W( 82 )
  JVS( 148) = W( 86 )
  IF ( ABS(  JVS( 149 )) < TINY(a) ) THEN
         IER = 34                                      
         RETURN
  END IF
   W( 34 ) = JVS( 149 )
   W( 82 ) = JVS( 150 )
  JVS( 149) = W( 34 )
  JVS( 150) = W( 82 )
  IF ( ABS(  JVS( 151 )) < TINY(a) ) THEN
         IER = 35                                      
         RETURN
  END IF
   W( 35 ) = JVS( 151 )
   W( 81 ) = JVS( 152 )
   W( 82 ) = JVS( 153 )
   W( 85 ) = JVS( 154 )
  JVS( 151) = W( 35 )
  JVS( 152) = W( 81 )
  JVS( 153) = W( 82 )
  JVS( 154) = W( 85 )
  IF ( ABS(  JVS( 155 )) < TINY(a) ) THEN
         IER = 36                                      
         RETURN
  END IF
   W( 36 ) = JVS( 155 )
   W( 68 ) = JVS( 156 )
   W( 78 ) = JVS( 157 )
   W( 85 ) = JVS( 158 )
  JVS( 155) = W( 36 )
  JVS( 156) = W( 68 )
  JVS( 157) = W( 78 )
  JVS( 158) = W( 85 )
  IF ( ABS(  JVS( 159 )) < TINY(a) ) THEN
         IER = 37                                      
         RETURN
  END IF
   W( 37 ) = JVS( 159 )
   W( 46 ) = JVS( 160 )
   W( 79 ) = JVS( 161 )
   W( 80 ) = JVS( 162 )
   W( 85 ) = JVS( 163 )
  JVS( 159) = W( 37 )
  JVS( 160) = W( 46 )
  JVS( 161) = W( 79 )
  JVS( 162) = W( 80 )
  JVS( 163) = W( 85 )
  IF ( ABS(  JVS( 164 )) < TINY(a) ) THEN
         IER = 38                                      
         RETURN
  END IF
   W( 38 ) = JVS( 164 )
   W( 79 ) = JVS( 165 )
   W( 82 ) = JVS( 166 )
   W( 85 ) = JVS( 167 )
  JVS( 164) = W( 38 )
  JVS( 165) = W( 79 )
  JVS( 166) = W( 82 )
  JVS( 167) = W( 85 )
  IF ( ABS(  JVS( 168 )) < TINY(a) ) THEN
         IER = 39                                      
         RETURN
  END IF
   W( 39 ) = JVS( 168 )
   W( 82 ) = JVS( 169 )
  JVS( 168) = W( 39 )
  JVS( 169) = W( 82 )
  IF ( ABS(  JVS( 170 )) < TINY(a) ) THEN
         IER = 40                                      
         RETURN
  END IF
   W( 40 ) = JVS( 170 )
   W( 82 ) = JVS( 171 )
  JVS( 170) = W( 40 )
  JVS( 171) = W( 82 )
  IF ( ABS(  JVS( 174 )) < TINY(a) ) THEN
         IER = 41                                      
         RETURN
  END IF
   W( 34 ) = JVS( 172 )
   W( 40 ) = JVS( 173 )
   W( 41 ) = JVS( 174 )
   W( 82 ) = JVS( 175 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  JVS( 172) = W( 34 )
  JVS( 173) = W( 40 )
  JVS( 174) = W( 41 )
  JVS( 175) = W( 82 )
  IF ( ABS(  JVS( 178 )) < TINY(a) ) THEN
         IER = 42                                      
         RETURN
  END IF
   W( 34 ) = JVS( 176 )
   W( 40 ) = JVS( 177 )
   W( 42 ) = JVS( 178 )
   W( 82 ) = JVS( 179 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  JVS( 176) = W( 34 )
  JVS( 177) = W( 40 )
  JVS( 178) = W( 42 )
  JVS( 179) = W( 82 )
  IF ( ABS(  JVS( 182 )) < TINY(a) ) THEN
         IER = 43                                      
         RETURN
  END IF
   W( 34 ) = JVS( 180 )
   W( 40 ) = JVS( 181 )
   W( 43 ) = JVS( 182 )
   W( 80 ) = JVS( 183 )
   W( 82 ) = JVS( 184 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  JVS( 180) = W( 34 )
  JVS( 181) = W( 40 )
  JVS( 182) = W( 43 )
  JVS( 183) = W( 80 )
  JVS( 184) = W( 82 )
  IF ( ABS(  JVS( 185 )) < TINY(a) ) THEN
         IER = 44                                      
         RETURN
  END IF
   W( 44 ) = JVS( 185 )
   W( 74 ) = JVS( 186 )
   W( 82 ) = JVS( 187 )
  JVS( 185) = W( 44 )
  JVS( 186) = W( 74 )
  JVS( 187) = W( 82 )
  IF ( ABS(  JVS( 190 )) < TINY(a) ) THEN
         IER = 45                                      
         RETURN
  END IF
   W( 34 ) = JVS( 188 )
   W( 40 ) = JVS( 189 )
   W( 45 ) = JVS( 190 )
   W( 74 ) = JVS( 191 )
   W( 82 ) = JVS( 192 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  JVS( 188) = W( 34 )
  JVS( 189) = W( 40 )
  JVS( 190) = W( 45 )
  JVS( 191) = W( 74 )
  JVS( 192) = W( 82 )
  IF ( ABS(  JVS( 194 )) < TINY(a) ) THEN
         IER = 46                                      
         RETURN
  END IF
   W( 37 ) = JVS( 193 )
   W( 46 ) = JVS( 194 )
   W( 57 ) = JVS( 195 )
   W( 79 ) = JVS( 196 )
   W( 80 ) = JVS( 197 )
   W( 85 ) = JVS( 198 )
  a = -W( 37 ) / JVS(          159  )
  W( 37 ) = -a
  W( 46 ) = W( 46 ) + a*JVS( 160 )
  W( 79 ) = W( 79 ) + a*JVS( 161 )
  W( 80 ) = W( 80 ) + a*JVS( 162 )
  W( 85 ) = W( 85 ) + a*JVS( 163 )
  JVS( 193) = W( 37 )
  JVS( 194) = W( 46 )
  JVS( 195) = W( 57 )
  JVS( 196) = W( 79 )
  JVS( 197) = W( 80 )
  JVS( 198) = W( 85 )
  IF ( ABS(  JVS( 201 )) < TINY(a) ) THEN
         IER = 47                                      
         RETURN
  END IF
   W( 34 ) = JVS( 199 )
   W( 40 ) = JVS( 200 )
   W( 47 ) = JVS( 201 )
   W( 65 ) = JVS( 202 )
   W( 74 ) = JVS( 203 )
   W( 80 ) = JVS( 204 )
   W( 82 ) = JVS( 205 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  JVS( 199) = W( 34 )
  JVS( 200) = W( 40 )
  JVS( 201) = W( 47 )
  JVS( 202) = W( 65 )
  JVS( 203) = W( 74 )
  JVS( 204) = W( 80 )
  JVS( 205) = W( 82 )
  IF ( ABS(  JVS( 206 )) < TINY(a) ) THEN
         IER = 48                                      
         RETURN
  END IF
   W( 48 ) = JVS( 206 )
   W( 77 ) = JVS( 207 )
   W( 82 ) = JVS( 208 )
   W( 85 ) = JVS( 209 )
   W( 86 ) = JVS( 210 )
  JVS( 206) = W( 48 )
  JVS( 207) = W( 77 )
  JVS( 208) = W( 82 )
  JVS( 209) = W( 85 )
  JVS( 210) = W( 86 )
  IF ( ABS(  JVS( 212 )) < TINY(a) ) THEN
         IER = 49                                      
         RETURN
  END IF
   W( 40 ) = JVS( 211 )
   W( 49 ) = JVS( 212 )
   W( 57 ) = JVS( 213 )
   W( 80 ) = JVS( 214 )
   W( 82 ) = JVS( 215 )
   W( 85 ) = JVS( 216 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  JVS( 211) = W( 40 )
  JVS( 212) = W( 49 )
  JVS( 213) = W( 57 )
  JVS( 214) = W( 80 )
  JVS( 215) = W( 82 )
  JVS( 216) = W( 85 )
  IF ( ABS(  JVS( 222 )) < TINY(a) ) THEN
         IER = 50                                      
         RETURN
  END IF
   W( 34 ) = JVS( 217 )
   W( 40 ) = JVS( 218 )
   W( 41 ) = JVS( 219 )
   W( 42 ) = JVS( 220 )
   W( 43 ) = JVS( 221 )
   W( 50 ) = JVS( 222 )
   W( 62 ) = JVS( 223 )
   W( 64 ) = JVS( 224 )
   W( 66 ) = JVS( 225 )
   W( 74 ) = JVS( 226 )
   W( 80 ) = JVS( 227 )
   W( 82 ) = JVS( 228 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 43 ) / JVS(          182  )
  W( 43 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 183 )
  W( 82 ) = W( 82 ) + a*JVS( 184 )
  JVS( 217) = W( 34 )
  JVS( 218) = W( 40 )
  JVS( 219) = W( 41 )
  JVS( 220) = W( 42 )
  JVS( 221) = W( 43 )
  JVS( 222) = W( 50 )
  JVS( 223) = W( 62 )
  JVS( 224) = W( 64 )
  JVS( 225) = W( 66 )
  JVS( 226) = W( 74 )
  JVS( 227) = W( 80 )
  JVS( 228) = W( 82 )
  IF ( ABS(  JVS( 235 )) < TINY(a) ) THEN
         IER = 51                                      
         RETURN
  END IF
   W( 39 ) = JVS( 229 )
   W( 41 ) = JVS( 230 )
   W( 42 ) = JVS( 231 )
   W( 44 ) = JVS( 232 )
   W( 45 ) = JVS( 233 )
   W( 50 ) = JVS( 234 )
   W( 51 ) = JVS( 235 )
   W( 53 ) = JVS( 236 )
   W( 55 ) = JVS( 237 )
   W( 56 ) = JVS( 238 )
   W( 58 ) = JVS( 239 )
   W( 59 ) = JVS( 240 )
   W( 61 ) = JVS( 241 )
   W( 62 ) = JVS( 242 )
   W( 63 ) = JVS( 243 )
   W( 64 ) = JVS( 244 )
   W( 65 ) = JVS( 245 )
   W( 66 ) = JVS( 246 )
   W( 67 ) = JVS( 247 )
   W( 68 ) = JVS( 248 )
   W( 70 ) = JVS( 249 )
   W( 71 ) = JVS( 250 )
   W( 74 ) = JVS( 251 )
   W( 80 ) = JVS( 252 )
   W( 82 ) = JVS( 253 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 44 ) / JVS(          185  )
  W( 44 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 186 )
  W( 82 ) = W( 82 ) + a*JVS( 187 )
  a = -W( 45 ) / JVS(          190  )
  W( 45 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 191 )
  W( 82 ) = W( 82 ) + a*JVS( 192 )
  a = -W( 50 ) / JVS(          222  )
  W( 50 ) = -a
  W( 62 ) = W( 62 ) + a*JVS( 223 )
  W( 64 ) = W( 64 ) + a*JVS( 224 )
  W( 66 ) = W( 66 ) + a*JVS( 225 )
  W( 74 ) = W( 74 ) + a*JVS( 226 )
  W( 80 ) = W( 80 ) + a*JVS( 227 )
  W( 82 ) = W( 82 ) + a*JVS( 228 )
  JVS( 229) = W( 39 )
  JVS( 230) = W( 41 )
  JVS( 231) = W( 42 )
  JVS( 232) = W( 44 )
  JVS( 233) = W( 45 )
  JVS( 234) = W( 50 )
  JVS( 235) = W( 51 )
  JVS( 236) = W( 53 )
  JVS( 237) = W( 55 )
  JVS( 238) = W( 56 )
  JVS( 239) = W( 58 )
  JVS( 240) = W( 59 )
  JVS( 241) = W( 61 )
  JVS( 242) = W( 62 )
  JVS( 243) = W( 63 )
  JVS( 244) = W( 64 )
  JVS( 245) = W( 65 )
  JVS( 246) = W( 66 )
  JVS( 247) = W( 67 )
  JVS( 248) = W( 68 )
  JVS( 249) = W( 70 )
  JVS( 250) = W( 71 )
  JVS( 251) = W( 74 )
  JVS( 252) = W( 80 )
  JVS( 253) = W( 82 )
  IF ( ABS(  JVS( 260 )) < TINY(a) ) THEN
         IER = 52                                      
         RETURN
  END IF
   W( 28 ) = JVS( 254 )
   W( 43 ) = JVS( 255 )
   W( 46 ) = JVS( 256 )
   W( 47 ) = JVS( 257 )
   W( 49 ) = JVS( 258 )
   W( 50 ) = JVS( 259 )
   W( 52 ) = JVS( 260 )
   W( 56 ) = JVS( 261 )
   W( 57 ) = JVS( 262 )
   W( 62 ) = JVS( 263 )
   W( 64 ) = JVS( 264 )
   W( 65 ) = JVS( 265 )
   W( 66 ) = JVS( 266 )
   W( 67 ) = JVS( 267 )
   W( 68 ) = JVS( 268 )
   W( 71 ) = JVS( 269 )
   W( 74 ) = JVS( 270 )
   W( 79 ) = JVS( 271 )
   W( 80 ) = JVS( 272 )
   W( 82 ) = JVS( 273 )
   W( 85 ) = JVS( 274 )
  a = -W( 28 ) / JVS(          130  )
  W( 28 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 131 )
  W( 80 ) = W( 80 ) + a*JVS( 132 )
  a = -W( 43 ) / JVS(          182  )
  W( 43 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 183 )
  W( 82 ) = W( 82 ) + a*JVS( 184 )
  a = -W( 46 ) / JVS(          194  )
  W( 46 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 195 )
  W( 79 ) = W( 79 ) + a*JVS( 196 )
  W( 80 ) = W( 80 ) + a*JVS( 197 )
  W( 85 ) = W( 85 ) + a*JVS( 198 )
  a = -W( 47 ) / JVS(          201  )
  W( 47 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 202 )
  W( 74 ) = W( 74 ) + a*JVS( 203 )
  W( 80 ) = W( 80 ) + a*JVS( 204 )
  W( 82 ) = W( 82 ) + a*JVS( 205 )
  a = -W( 49 ) / JVS(          212  )
  W( 49 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 213 )
  W( 80 ) = W( 80 ) + a*JVS( 214 )
  W( 82 ) = W( 82 ) + a*JVS( 215 )
  W( 85 ) = W( 85 ) + a*JVS( 216 )
  a = -W( 50 ) / JVS(          222  )
  W( 50 ) = -a
  W( 62 ) = W( 62 ) + a*JVS( 223 )
  W( 64 ) = W( 64 ) + a*JVS( 224 )
  W( 66 ) = W( 66 ) + a*JVS( 225 )
  W( 74 ) = W( 74 ) + a*JVS( 226 )
  W( 80 ) = W( 80 ) + a*JVS( 227 )
  W( 82 ) = W( 82 ) + a*JVS( 228 )
  JVS( 254) = W( 28 )
  JVS( 255) = W( 43 )
  JVS( 256) = W( 46 )
  JVS( 257) = W( 47 )
  JVS( 258) = W( 49 )
  JVS( 259) = W( 50 )
  JVS( 260) = W( 52 )
  JVS( 261) = W( 56 )
  JVS( 262) = W( 57 )
  JVS( 263) = W( 62 )
  JVS( 264) = W( 64 )
  JVS( 265) = W( 65 )
  JVS( 266) = W( 66 )
  JVS( 267) = W( 67 )
  JVS( 268) = W( 68 )
  JVS( 269) = W( 71 )
  JVS( 270) = W( 74 )
  JVS( 271) = W( 79 )
  JVS( 272) = W( 80 )
  JVS( 273) = W( 82 )
  JVS( 274) = W( 85 )
  IF ( ABS(  JVS( 275 )) < TINY(a) ) THEN
         IER = 53                                      
         RETURN
  END IF
   W( 53 ) = JVS( 275 )
   W( 70 ) = JVS( 276 )
   W( 74 ) = JVS( 277 )
   W( 80 ) = JVS( 278 )
   W( 82 ) = JVS( 279 )
  JVS( 275) = W( 53 )
  JVS( 276) = W( 70 )
  JVS( 277) = W( 74 )
  JVS( 278) = W( 80 )
  JVS( 279) = W( 82 )
  IF ( ABS(  JVS( 285 )) < TINY(a) ) THEN
         IER = 54                                      
         RETURN
  END IF
   W( 26 ) = JVS( 280 )
   W( 30 ) = JVS( 281 )
   W( 31 ) = JVS( 282 )
   W( 32 ) = JVS( 283 )
   W( 39 ) = JVS( 284 )
   W( 54 ) = JVS( 285 )
   W( 59 ) = JVS( 286 )
   W( 61 ) = JVS( 287 )
   W( 63 ) = JVS( 288 )
   W( 65 ) = JVS( 289 )
   W( 69 ) = JVS( 290 )
   W( 74 ) = JVS( 291 )
   W( 79 ) = JVS( 292 )
   W( 80 ) = JVS( 293 )
   W( 82 ) = JVS( 294 )
  a = -W( 26 ) / JVS(          122  )
  W( 26 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 123 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 31 ) / JVS(          139  )
  W( 31 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 140 )
  W( 82 ) = W( 82 ) + a*JVS( 141 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  JVS( 280) = W( 26 )
  JVS( 281) = W( 30 )
  JVS( 282) = W( 31 )
  JVS( 283) = W( 32 )
  JVS( 284) = W( 39 )
  JVS( 285) = W( 54 )
  JVS( 286) = W( 59 )
  JVS( 287) = W( 61 )
  JVS( 288) = W( 63 )
  JVS( 289) = W( 65 )
  JVS( 290) = W( 69 )
  JVS( 291) = W( 74 )
  JVS( 292) = W( 79 )
  JVS( 293) = W( 80 )
  JVS( 294) = W( 82 )
  IF ( ABS(  JVS( 295 )) < TINY(a) ) THEN
         IER = 55                                      
         RETURN
  END IF
   W( 55 ) = JVS( 295 )
   W( 70 ) = JVS( 296 )
   W( 74 ) = JVS( 297 )
   W( 80 ) = JVS( 298 )
   W( 82 ) = JVS( 299 )
  JVS( 295) = W( 55 )
  JVS( 296) = W( 70 )
  JVS( 297) = W( 74 )
  JVS( 298) = W( 80 )
  JVS( 299) = W( 82 )
  IF ( ABS(  JVS( 308 )) < TINY(a) ) THEN
         IER = 56                                      
         RETURN
  END IF
   W( 34 ) = JVS( 300 )
   W( 40 ) = JVS( 301 )
   W( 41 ) = JVS( 302 )
   W( 42 ) = JVS( 303 )
   W( 44 ) = JVS( 304 )
   W( 45 ) = JVS( 305 )
   W( 49 ) = JVS( 306 )
   W( 53 ) = JVS( 307 )
   W( 56 ) = JVS( 308 )
   W( 57 ) = JVS( 309 )
   W( 59 ) = JVS( 310 )
   W( 61 ) = JVS( 311 )
   W( 64 ) = JVS( 312 )
   W( 70 ) = JVS( 313 )
   W( 74 ) = JVS( 314 )
   W( 80 ) = JVS( 315 )
   W( 82 ) = JVS( 316 )
   W( 85 ) = JVS( 317 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 44 ) / JVS(          185  )
  W( 44 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 186 )
  W( 82 ) = W( 82 ) + a*JVS( 187 )
  a = -W( 45 ) / JVS(          190  )
  W( 45 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 191 )
  W( 82 ) = W( 82 ) + a*JVS( 192 )
  a = -W( 49 ) / JVS(          212  )
  W( 49 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 213 )
  W( 80 ) = W( 80 ) + a*JVS( 214 )
  W( 82 ) = W( 82 ) + a*JVS( 215 )
  W( 85 ) = W( 85 ) + a*JVS( 216 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  JVS( 300) = W( 34 )
  JVS( 301) = W( 40 )
  JVS( 302) = W( 41 )
  JVS( 303) = W( 42 )
  JVS( 304) = W( 44 )
  JVS( 305) = W( 45 )
  JVS( 306) = W( 49 )
  JVS( 307) = W( 53 )
  JVS( 308) = W( 56 )
  JVS( 309) = W( 57 )
  JVS( 310) = W( 59 )
  JVS( 311) = W( 61 )
  JVS( 312) = W( 64 )
  JVS( 313) = W( 70 )
  JVS( 314) = W( 74 )
  JVS( 315) = W( 80 )
  JVS( 316) = W( 82 )
  JVS( 317) = W( 85 )
  IF ( ABS(  JVS( 320 )) < TINY(a) ) THEN
         IER = 57                                      
         RETURN
  END IF
   W( 43 ) = JVS( 318 )
   W( 49 ) = JVS( 319 )
   W( 57 ) = JVS( 320 )
   W( 75 ) = JVS( 321 )
   W( 76 ) = JVS( 322 )
   W( 78 ) = JVS( 323 )
   W( 79 ) = JVS( 324 )
   W( 80 ) = JVS( 325 )
   W( 82 ) = JVS( 326 )
   W( 83 ) = JVS( 327 )
   W( 84 ) = JVS( 328 )
   W( 85 ) = JVS( 329 )
  a = -W( 43 ) / JVS(          182  )
  W( 43 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 183 )
  W( 82 ) = W( 82 ) + a*JVS( 184 )
  a = -W( 49 ) / JVS(          212  )
  W( 49 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 213 )
  W( 80 ) = W( 80 ) + a*JVS( 214 )
  W( 82 ) = W( 82 ) + a*JVS( 215 )
  W( 85 ) = W( 85 ) + a*JVS( 216 )
  JVS( 318) = W( 43 )
  JVS( 319) = W( 49 )
  JVS( 320) = W( 57 )
  JVS( 321) = W( 75 )
  JVS( 322) = W( 76 )
  JVS( 323) = W( 78 )
  JVS( 324) = W( 79 )
  JVS( 325) = W( 80 )
  JVS( 326) = W( 82 )
  JVS( 327) = W( 83 )
  JVS( 328) = W( 84 )
  JVS( 329) = W( 85 )
  IF ( ABS(  JVS( 330 )) < TINY(a) ) THEN
         IER = 58                                      
         RETURN
  END IF
   W( 58 ) = JVS( 330 )
   W( 70 ) = JVS( 331 )
   W( 74 ) = JVS( 332 )
   W( 80 ) = JVS( 333 )
   W( 82 ) = JVS( 334 )
  JVS( 330) = W( 58 )
  JVS( 331) = W( 70 )
  JVS( 332) = W( 74 )
  JVS( 333) = W( 80 )
  JVS( 334) = W( 82 )
  IF ( ABS(  JVS( 335 )) < TINY(a) ) THEN
         IER = 59                                      
         RETURN
  END IF
   W( 59 ) = JVS( 335 )
   W( 70 ) = JVS( 336 )
   W( 74 ) = JVS( 337 )
   W( 80 ) = JVS( 338 )
   W( 82 ) = JVS( 339 )
  JVS( 335) = W( 59 )
  JVS( 336) = W( 70 )
  JVS( 337) = W( 74 )
  JVS( 338) = W( 80 )
  JVS( 339) = W( 82 )
  IF ( ABS(  JVS( 348 )) < TINY(a) ) THEN
         IER = 60                                      
         RETURN
  END IF
   W( 30 ) = JVS( 340 )
   W( 32 ) = JVS( 341 )
   W( 39 ) = JVS( 342 )
   W( 41 ) = JVS( 343 )
   W( 42 ) = JVS( 344 )
   W( 54 ) = JVS( 345 )
   W( 58 ) = JVS( 346 )
   W( 59 ) = JVS( 347 )
   W( 60 ) = JVS( 348 )
   W( 61 ) = JVS( 349 )
   W( 63 ) = JVS( 350 )
   W( 65 ) = JVS( 351 )
   W( 66 ) = JVS( 352 )
   W( 69 ) = JVS( 353 )
   W( 70 ) = JVS( 354 )
   W( 72 ) = JVS( 355 )
   W( 73 ) = JVS( 356 )
   W( 74 ) = JVS( 357 )
   W( 75 ) = JVS( 358 )
   W( 76 ) = JVS( 359 )
   W( 77 ) = JVS( 360 )
   W( 78 ) = JVS( 361 )
   W( 79 ) = JVS( 362 )
   W( 80 ) = JVS( 363 )
   W( 81 ) = JVS( 364 )
   W( 82 ) = JVS( 365 )
   W( 83 ) = JVS( 366 )
   W( 84 ) = JVS( 367 )
   W( 85 ) = JVS( 368 )
   W( 86 ) = JVS( 369 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 54 ) / JVS(          285  )
  W( 54 ) = -a
  W( 59 ) = W( 59 ) + a*JVS( 286 )
  W( 61 ) = W( 61 ) + a*JVS( 287 )
  W( 63 ) = W( 63 ) + a*JVS( 288 )
  W( 65 ) = W( 65 ) + a*JVS( 289 )
  W( 69 ) = W( 69 ) + a*JVS( 290 )
  W( 74 ) = W( 74 ) + a*JVS( 291 )
  W( 79 ) = W( 79 ) + a*JVS( 292 )
  W( 80 ) = W( 80 ) + a*JVS( 293 )
  W( 82 ) = W( 82 ) + a*JVS( 294 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  JVS( 340) = W( 30 )
  JVS( 341) = W( 32 )
  JVS( 342) = W( 39 )
  JVS( 343) = W( 41 )
  JVS( 344) = W( 42 )
  JVS( 345) = W( 54 )
  JVS( 346) = W( 58 )
  JVS( 347) = W( 59 )
  JVS( 348) = W( 60 )
  JVS( 349) = W( 61 )
  JVS( 350) = W( 63 )
  JVS( 351) = W( 65 )
  JVS( 352) = W( 66 )
  JVS( 353) = W( 69 )
  JVS( 354) = W( 70 )
  JVS( 355) = W( 72 )
  JVS( 356) = W( 73 )
  JVS( 357) = W( 74 )
  JVS( 358) = W( 75 )
  JVS( 359) = W( 76 )
  JVS( 360) = W( 77 )
  JVS( 361) = W( 78 )
  JVS( 362) = W( 79 )
  JVS( 363) = W( 80 )
  JVS( 364) = W( 81 )
  JVS( 365) = W( 82 )
  JVS( 366) = W( 83 )
  JVS( 367) = W( 84 )
  JVS( 368) = W( 85 )
  JVS( 369) = W( 86 )
  IF ( ABS(  JVS( 370 )) < TINY(a) ) THEN
         IER = 61                                      
         RETURN
  END IF
   W( 61 ) = JVS( 370 )
   W( 70 ) = JVS( 371 )
   W( 74 ) = JVS( 372 )
   W( 80 ) = JVS( 373 )
   W( 82 ) = JVS( 374 )
  JVS( 370) = W( 61 )
  JVS( 371) = W( 70 )
  JVS( 372) = W( 74 )
  JVS( 373) = W( 80 )
  JVS( 374) = W( 82 )
  IF ( ABS(  JVS( 376 )) < TINY(a) ) THEN
         IER = 62                                      
         RETURN
  END IF
   W( 58 ) = JVS( 375 )
   W( 62 ) = JVS( 376 )
   W( 65 ) = JVS( 377 )
   W( 70 ) = JVS( 378 )
   W( 74 ) = JVS( 379 )
   W( 80 ) = JVS( 380 )
   W( 82 ) = JVS( 381 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  JVS( 375) = W( 58 )
  JVS( 376) = W( 62 )
  JVS( 377) = W( 65 )
  JVS( 378) = W( 70 )
  JVS( 379) = W( 74 )
  JVS( 380) = W( 80 )
  JVS( 381) = W( 82 )
  IF ( ABS(  JVS( 382 )) < TINY(a) ) THEN
         IER = 63                                      
         RETURN
  END IF
   W( 63 ) = JVS( 382 )
   W( 70 ) = JVS( 383 )
   W( 74 ) = JVS( 384 )
   W( 80 ) = JVS( 385 )
   W( 82 ) = JVS( 386 )
  JVS( 382) = W( 63 )
  JVS( 383) = W( 70 )
  JVS( 384) = W( 74 )
  JVS( 385) = W( 80 )
  JVS( 386) = W( 82 )
  IF ( ABS(  JVS( 388 )) < TINY(a) ) THEN
         IER = 64                                      
         RETURN
  END IF
   W( 58 ) = JVS( 387 )
   W( 64 ) = JVS( 388 )
   W( 65 ) = JVS( 389 )
   W( 70 ) = JVS( 390 )
   W( 74 ) = JVS( 391 )
   W( 80 ) = JVS( 392 )
   W( 82 ) = JVS( 393 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  JVS( 387) = W( 58 )
  JVS( 388) = W( 64 )
  JVS( 389) = W( 65 )
  JVS( 390) = W( 70 )
  JVS( 391) = W( 74 )
  JVS( 392) = W( 80 )
  JVS( 393) = W( 82 )
  IF ( ABS(  JVS( 394 )) < TINY(a) ) THEN
         IER = 65                                      
         RETURN
  END IF
   W( 65 ) = JVS( 394 )
   W( 70 ) = JVS( 395 )
   W( 74 ) = JVS( 396 )
   W( 80 ) = JVS( 397 )
   W( 82 ) = JVS( 398 )
  JVS( 394) = W( 65 )
  JVS( 395) = W( 70 )
  JVS( 396) = W( 74 )
  JVS( 397) = W( 80 )
  JVS( 398) = W( 82 )
  IF ( ABS(  JVS( 401 )) < TINY(a) ) THEN
         IER = 66                                      
         RETURN
  END IF
   W( 58 ) = JVS( 399 )
   W( 65 ) = JVS( 400 )
   W( 66 ) = JVS( 401 )
   W( 70 ) = JVS( 402 )
   W( 74 ) = JVS( 403 )
   W( 80 ) = JVS( 404 )
   W( 82 ) = JVS( 405 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  JVS( 399) = W( 58 )
  JVS( 400) = W( 65 )
  JVS( 401) = W( 66 )
  JVS( 402) = W( 70 )
  JVS( 403) = W( 74 )
  JVS( 404) = W( 80 )
  JVS( 405) = W( 82 )
  IF ( ABS(  JVS( 416 )) < TINY(a) ) THEN
         IER = 67                                      
         RETURN
  END IF
   W( 19 ) = JVS( 406 )
   W( 25 ) = JVS( 407 )
   W( 30 ) = JVS( 408 )
   W( 32 ) = JVS( 409 )
   W( 39 ) = JVS( 410 )
   W( 53 ) = JVS( 411 )
   W( 55 ) = JVS( 412 )
   W( 63 ) = JVS( 413 )
   W( 64 ) = JVS( 414 )
   W( 65 ) = JVS( 415 )
   W( 67 ) = JVS( 416 )
   W( 69 ) = JVS( 417 )
   W( 70 ) = JVS( 418 )
   W( 71 ) = JVS( 419 )
   W( 72 ) = JVS( 420 )
   W( 73 ) = JVS( 421 )
   W( 74 ) = JVS( 422 )
   W( 75 ) = JVS( 423 )
   W( 76 ) = JVS( 424 )
   W( 78 ) = JVS( 425 )
   W( 80 ) = JVS( 426 )
   W( 82 ) = JVS( 427 )
   W( 83 ) = JVS( 428 )
   W( 84 ) = JVS( 429 )
  a = -W( 19 ) / JVS(          103  )
  W( 19 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 104 )
  a = -W( 25 ) / JVS(          120  )
  W( 25 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 121 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  JVS( 406) = W( 19 )
  JVS( 407) = W( 25 )
  JVS( 408) = W( 30 )
  JVS( 409) = W( 32 )
  JVS( 410) = W( 39 )
  JVS( 411) = W( 53 )
  JVS( 412) = W( 55 )
  JVS( 413) = W( 63 )
  JVS( 414) = W( 64 )
  JVS( 415) = W( 65 )
  JVS( 416) = W( 67 )
  JVS( 417) = W( 69 )
  JVS( 418) = W( 70 )
  JVS( 419) = W( 71 )
  JVS( 420) = W( 72 )
  JVS( 421) = W( 73 )
  JVS( 422) = W( 74 )
  JVS( 423) = W( 75 )
  JVS( 424) = W( 76 )
  JVS( 425) = W( 78 )
  JVS( 426) = W( 80 )
  JVS( 427) = W( 82 )
  JVS( 428) = W( 83 )
  JVS( 429) = W( 84 )
  IF ( ABS(  JVS( 451 )) < TINY(a) ) THEN
         IER = 68                                      
         RETURN
  END IF
   W( 25 ) = JVS( 430 )
   W( 30 ) = JVS( 431 )
   W( 32 ) = JVS( 432 )
   W( 33 ) = JVS( 433 )
   W( 35 ) = JVS( 434 )
   W( 36 ) = JVS( 435 )
   W( 39 ) = JVS( 436 )
   W( 44 ) = JVS( 437 )
   W( 53 ) = JVS( 438 )
   W( 54 ) = JVS( 439 )
   W( 55 ) = JVS( 440 )
   W( 56 ) = JVS( 441 )
   W( 57 ) = JVS( 442 )
   W( 58 ) = JVS( 443 )
   W( 59 ) = JVS( 444 )
   W( 61 ) = JVS( 445 )
   W( 62 ) = JVS( 446 )
   W( 63 ) = JVS( 447 )
   W( 64 ) = JVS( 448 )
   W( 65 ) = JVS( 449 )
   W( 66 ) = JVS( 450 )
   W( 68 ) = JVS( 451 )
   W( 69 ) = JVS( 452 )
   W( 70 ) = JVS( 453 )
   W( 72 ) = JVS( 454 )
   W( 73 ) = JVS( 455 )
   W( 74 ) = JVS( 456 )
   W( 75 ) = JVS( 457 )
   W( 76 ) = JVS( 458 )
   W( 77 ) = JVS( 459 )
   W( 78 ) = JVS( 460 )
   W( 79 ) = JVS( 461 )
   W( 80 ) = JVS( 462 )
   W( 81 ) = JVS( 463 )
   W( 82 ) = JVS( 464 )
   W( 83 ) = JVS( 465 )
   W( 84 ) = JVS( 466 )
   W( 85 ) = JVS( 467 )
   W( 86 ) = JVS( 468 )
  a = -W( 25 ) / JVS(          120  )
  W( 25 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 121 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 33 ) / JVS(          144  )
  W( 33 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 145 )
  W( 81 ) = W( 81 ) + a*JVS( 146 )
  W( 82 ) = W( 82 ) + a*JVS( 147 )
  W( 86 ) = W( 86 ) + a*JVS( 148 )
  a = -W( 35 ) / JVS(          151  )
  W( 35 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 152 )
  W( 82 ) = W( 82 ) + a*JVS( 153 )
  W( 85 ) = W( 85 ) + a*JVS( 154 )
  a = -W( 36 ) / JVS(          155  )
  W( 36 ) = -a
  W( 68 ) = W( 68 ) + a*JVS( 156 )
  W( 78 ) = W( 78 ) + a*JVS( 157 )
  W( 85 ) = W( 85 ) + a*JVS( 158 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 44 ) / JVS(          185  )
  W( 44 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 186 )
  W( 82 ) = W( 82 ) + a*JVS( 187 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 54 ) / JVS(          285  )
  W( 54 ) = -a
  W( 59 ) = W( 59 ) + a*JVS( 286 )
  W( 61 ) = W( 61 ) + a*JVS( 287 )
  W( 63 ) = W( 63 ) + a*JVS( 288 )
  W( 65 ) = W( 65 ) + a*JVS( 289 )
  W( 69 ) = W( 69 ) + a*JVS( 290 )
  W( 74 ) = W( 74 ) + a*JVS( 291 )
  W( 79 ) = W( 79 ) + a*JVS( 292 )
  W( 80 ) = W( 80 ) + a*JVS( 293 )
  W( 82 ) = W( 82 ) + a*JVS( 294 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 56 ) / JVS(          308  )
  W( 56 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 309 )
  W( 59 ) = W( 59 ) + a*JVS( 310 )
  W( 61 ) = W( 61 ) + a*JVS( 311 )
  W( 64 ) = W( 64 ) + a*JVS( 312 )
  W( 70 ) = W( 70 ) + a*JVS( 313 )
  W( 74 ) = W( 74 ) + a*JVS( 314 )
  W( 80 ) = W( 80 ) + a*JVS( 315 )
  W( 82 ) = W( 82 ) + a*JVS( 316 )
  W( 85 ) = W( 85 ) + a*JVS( 317 )
  a = -W( 57 ) / JVS(          320  )
  W( 57 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 321 )
  W( 76 ) = W( 76 ) + a*JVS( 322 )
  W( 78 ) = W( 78 ) + a*JVS( 323 )
  W( 79 ) = W( 79 ) + a*JVS( 324 )
  W( 80 ) = W( 80 ) + a*JVS( 325 )
  W( 82 ) = W( 82 ) + a*JVS( 326 )
  W( 83 ) = W( 83 ) + a*JVS( 327 )
  W( 84 ) = W( 84 ) + a*JVS( 328 )
  W( 85 ) = W( 85 ) + a*JVS( 329 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  JVS( 430) = W( 25 )
  JVS( 431) = W( 30 )
  JVS( 432) = W( 32 )
  JVS( 433) = W( 33 )
  JVS( 434) = W( 35 )
  JVS( 435) = W( 36 )
  JVS( 436) = W( 39 )
  JVS( 437) = W( 44 )
  JVS( 438) = W( 53 )
  JVS( 439) = W( 54 )
  JVS( 440) = W( 55 )
  JVS( 441) = W( 56 )
  JVS( 442) = W( 57 )
  JVS( 443) = W( 58 )
  JVS( 444) = W( 59 )
  JVS( 445) = W( 61 )
  JVS( 446) = W( 62 )
  JVS( 447) = W( 63 )
  JVS( 448) = W( 64 )
  JVS( 449) = W( 65 )
  JVS( 450) = W( 66 )
  JVS( 451) = W( 68 )
  JVS( 452) = W( 69 )
  JVS( 453) = W( 70 )
  JVS( 454) = W( 72 )
  JVS( 455) = W( 73 )
  JVS( 456) = W( 74 )
  JVS( 457) = W( 75 )
  JVS( 458) = W( 76 )
  JVS( 459) = W( 77 )
  JVS( 460) = W( 78 )
  JVS( 461) = W( 79 )
  JVS( 462) = W( 80 )
  JVS( 463) = W( 81 )
  JVS( 464) = W( 82 )
  JVS( 465) = W( 83 )
  JVS( 466) = W( 84 )
  JVS( 467) = W( 85 )
  JVS( 468) = W( 86 )
  IF ( ABS(  JVS( 475 )) < TINY(a) ) THEN
         IER = 69                                      
         RETURN
  END IF
   W( 31 ) = JVS( 469 )
   W( 59 ) = JVS( 470 )
   W( 61 ) = JVS( 471 )
   W( 63 ) = JVS( 472 )
   W( 64 ) = JVS( 473 )
   W( 65 ) = JVS( 474 )
   W( 69 ) = JVS( 475 )
   W( 70 ) = JVS( 476 )
   W( 74 ) = JVS( 477 )
   W( 78 ) = JVS( 478 )
   W( 79 ) = JVS( 479 )
   W( 80 ) = JVS( 480 )
   W( 82 ) = JVS( 481 )
   W( 86 ) = JVS( 482 )
  a = -W( 31 ) / JVS(          139  )
  W( 31 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 140 )
  W( 82 ) = W( 82 ) + a*JVS( 141 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  JVS( 469) = W( 31 )
  JVS( 470) = W( 59 )
  JVS( 471) = W( 61 )
  JVS( 472) = W( 63 )
  JVS( 473) = W( 64 )
  JVS( 474) = W( 65 )
  JVS( 475) = W( 69 )
  JVS( 476) = W( 70 )
  JVS( 477) = W( 74 )
  JVS( 478) = W( 78 )
  JVS( 479) = W( 79 )
  JVS( 480) = W( 80 )
  JVS( 481) = W( 82 )
  JVS( 482) = W( 86 )
  IF ( ABS(  JVS( 493 )) < TINY(a) ) THEN
         IER = 70                                      
         RETURN
  END IF
   W( 14 ) = JVS( 483 )
   W( 53 ) = JVS( 484 )
   W( 55 ) = JVS( 485 )
   W( 58 ) = JVS( 486 )
   W( 59 ) = JVS( 487 )
   W( 61 ) = JVS( 488 )
   W( 62 ) = JVS( 489 )
   W( 63 ) = JVS( 490 )
   W( 65 ) = JVS( 491 )
   W( 66 ) = JVS( 492 )
   W( 70 ) = JVS( 493 )
   W( 74 ) = JVS( 494 )
   W( 78 ) = JVS( 495 )
   W( 79 ) = JVS( 496 )
   W( 80 ) = JVS( 497 )
   W( 82 ) = JVS( 498 )
  a = -W( 14 ) / JVS(           87  )
  W( 14 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 88 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  JVS( 483) = W( 14 )
  JVS( 484) = W( 53 )
  JVS( 485) = W( 55 )
  JVS( 486) = W( 58 )
  JVS( 487) = W( 59 )
  JVS( 488) = W( 61 )
  JVS( 489) = W( 62 )
  JVS( 490) = W( 63 )
  JVS( 491) = W( 65 )
  JVS( 492) = W( 66 )
  JVS( 493) = W( 70 )
  JVS( 494) = W( 74 )
  JVS( 495) = W( 78 )
  JVS( 496) = W( 79 )
  JVS( 497) = W( 80 )
  JVS( 498) = W( 82 )
  IF ( ABS(  JVS( 518 )) < TINY(a) ) THEN
         IER = 71                                      
         RETURN
  END IF
   W( 26 ) = JVS( 499 )
   W( 30 ) = JVS( 500 )
   W( 32 ) = JVS( 501 )
   W( 39 ) = JVS( 502 )
   W( 41 ) = JVS( 503 )
   W( 42 ) = JVS( 504 )
   W( 45 ) = JVS( 505 )
   W( 48 ) = JVS( 506 )
   W( 53 ) = JVS( 507 )
   W( 55 ) = JVS( 508 )
   W( 59 ) = JVS( 509 )
   W( 61 ) = JVS( 510 )
   W( 62 ) = JVS( 511 )
   W( 63 ) = JVS( 512 )
   W( 64 ) = JVS( 513 )
   W( 65 ) = JVS( 514 )
   W( 66 ) = JVS( 515 )
   W( 69 ) = JVS( 516 )
   W( 70 ) = JVS( 517 )
   W( 71 ) = JVS( 518 )
   W( 72 ) = JVS( 519 )
   W( 73 ) = JVS( 520 )
   W( 74 ) = JVS( 521 )
   W( 77 ) = JVS( 522 )
   W( 78 ) = JVS( 523 )
   W( 79 ) = JVS( 524 )
   W( 80 ) = JVS( 525 )
   W( 82 ) = JVS( 526 )
   W( 85 ) = JVS( 527 )
   W( 86 ) = JVS( 528 )
  a = -W( 26 ) / JVS(          122  )
  W( 26 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 123 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 45 ) / JVS(          190  )
  W( 45 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 191 )
  W( 82 ) = W( 82 ) + a*JVS( 192 )
  a = -W( 48 ) / JVS(          206  )
  W( 48 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 207 )
  W( 82 ) = W( 82 ) + a*JVS( 208 )
  W( 85 ) = W( 85 ) + a*JVS( 209 )
  W( 86 ) = W( 86 ) + a*JVS( 210 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  JVS( 499) = W( 26 )
  JVS( 500) = W( 30 )
  JVS( 501) = W( 32 )
  JVS( 502) = W( 39 )
  JVS( 503) = W( 41 )
  JVS( 504) = W( 42 )
  JVS( 505) = W( 45 )
  JVS( 506) = W( 48 )
  JVS( 507) = W( 53 )
  JVS( 508) = W( 55 )
  JVS( 509) = W( 59 )
  JVS( 510) = W( 61 )
  JVS( 511) = W( 62 )
  JVS( 512) = W( 63 )
  JVS( 513) = W( 64 )
  JVS( 514) = W( 65 )
  JVS( 515) = W( 66 )
  JVS( 516) = W( 69 )
  JVS( 517) = W( 70 )
  JVS( 518) = W( 71 )
  JVS( 519) = W( 72 )
  JVS( 520) = W( 73 )
  JVS( 521) = W( 74 )
  JVS( 522) = W( 77 )
  JVS( 523) = W( 78 )
  JVS( 524) = W( 79 )
  JVS( 525) = W( 80 )
  JVS( 526) = W( 82 )
  JVS( 527) = W( 85 )
  JVS( 528) = W( 86 )
  IF ( ABS(  JVS( 540 )) < TINY(a) ) THEN
         IER = 72                                      
         RETURN
  END IF
   W( 30 ) = JVS( 529 )
   W( 32 ) = JVS( 530 )
   W( 39 ) = JVS( 531 )
   W( 55 ) = JVS( 532 )
   W( 62 ) = JVS( 533 )
   W( 63 ) = JVS( 534 )
   W( 64 ) = JVS( 535 )
   W( 65 ) = JVS( 536 )
   W( 66 ) = JVS( 537 )
   W( 69 ) = JVS( 538 )
   W( 70 ) = JVS( 539 )
   W( 72 ) = JVS( 540 )
   W( 73 ) = JVS( 541 )
   W( 74 ) = JVS( 542 )
   W( 77 ) = JVS( 543 )
   W( 78 ) = JVS( 544 )
   W( 79 ) = JVS( 545 )
   W( 80 ) = JVS( 546 )
   W( 81 ) = JVS( 547 )
   W( 82 ) = JVS( 548 )
   W( 86 ) = JVS( 549 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  JVS( 529) = W( 30 )
  JVS( 530) = W( 32 )
  JVS( 531) = W( 39 )
  JVS( 532) = W( 55 )
  JVS( 533) = W( 62 )
  JVS( 534) = W( 63 )
  JVS( 535) = W( 64 )
  JVS( 536) = W( 65 )
  JVS( 537) = W( 66 )
  JVS( 538) = W( 69 )
  JVS( 539) = W( 70 )
  JVS( 540) = W( 72 )
  JVS( 541) = W( 73 )
  JVS( 542) = W( 74 )
  JVS( 543) = W( 77 )
  JVS( 544) = W( 78 )
  JVS( 545) = W( 79 )
  JVS( 546) = W( 80 )
  JVS( 547) = W( 81 )
  JVS( 548) = W( 82 )
  JVS( 549) = W( 86 )
  IF ( ABS(  JVS( 562 )) < TINY(a) ) THEN
         IER = 73                                      
         RETURN
  END IF
   W( 32 ) = JVS( 550 )
   W( 39 ) = JVS( 551 )
   W( 40 ) = JVS( 552 )
   W( 58 ) = JVS( 553 )
   W( 59 ) = JVS( 554 )
   W( 61 ) = JVS( 555 )
   W( 63 ) = JVS( 556 )
   W( 64 ) = JVS( 557 )
   W( 65 ) = JVS( 558 )
   W( 66 ) = JVS( 559 )
   W( 69 ) = JVS( 560 )
   W( 70 ) = JVS( 561 )
   W( 73 ) = JVS( 562 )
   W( 74 ) = JVS( 563 )
   W( 76 ) = JVS( 564 )
   W( 77 ) = JVS( 565 )
   W( 78 ) = JVS( 566 )
   W( 79 ) = JVS( 567 )
   W( 80 ) = JVS( 568 )
   W( 81 ) = JVS( 569 )
   W( 82 ) = JVS( 570 )
   W( 83 ) = JVS( 571 )
   W( 84 ) = JVS( 572 )
   W( 86 ) = JVS( 573 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  JVS( 550) = W( 32 )
  JVS( 551) = W( 39 )
  JVS( 552) = W( 40 )
  JVS( 553) = W( 58 )
  JVS( 554) = W( 59 )
  JVS( 555) = W( 61 )
  JVS( 556) = W( 63 )
  JVS( 557) = W( 64 )
  JVS( 558) = W( 65 )
  JVS( 559) = W( 66 )
  JVS( 560) = W( 69 )
  JVS( 561) = W( 70 )
  JVS( 562) = W( 73 )
  JVS( 563) = W( 74 )
  JVS( 564) = W( 76 )
  JVS( 565) = W( 77 )
  JVS( 566) = W( 78 )
  JVS( 567) = W( 79 )
  JVS( 568) = W( 80 )
  JVS( 569) = W( 81 )
  JVS( 570) = W( 82 )
  JVS( 571) = W( 83 )
  JVS( 572) = W( 84 )
  JVS( 573) = W( 86 )
  IF ( ABS(  JVS( 587 )) < TINY(a) ) THEN
         IER = 74                                      
         RETURN
  END IF
   W( 44 ) = JVS( 574 )
   W( 45 ) = JVS( 575 )
   W( 53 ) = JVS( 576 )
   W( 55 ) = JVS( 577 )
   W( 58 ) = JVS( 578 )
   W( 59 ) = JVS( 579 )
   W( 61 ) = JVS( 580 )
   W( 62 ) = JVS( 581 )
   W( 63 ) = JVS( 582 )
   W( 64 ) = JVS( 583 )
   W( 65 ) = JVS( 584 )
   W( 66 ) = JVS( 585 )
   W( 70 ) = JVS( 586 )
   W( 74 ) = JVS( 587 )
   W( 75 ) = JVS( 588 )
   W( 76 ) = JVS( 589 )
   W( 78 ) = JVS( 590 )
   W( 79 ) = JVS( 591 )
   W( 80 ) = JVS( 592 )
   W( 82 ) = JVS( 593 )
   W( 83 ) = JVS( 594 )
   W( 84 ) = JVS( 595 )
   W( 85 ) = JVS( 596 )
  a = -W( 44 ) / JVS(          185  )
  W( 44 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 186 )
  W( 82 ) = W( 82 ) + a*JVS( 187 )
  a = -W( 45 ) / JVS(          190  )
  W( 45 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 191 )
  W( 82 ) = W( 82 ) + a*JVS( 192 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  JVS( 574) = W( 44 )
  JVS( 575) = W( 45 )
  JVS( 576) = W( 53 )
  JVS( 577) = W( 55 )
  JVS( 578) = W( 58 )
  JVS( 579) = W( 59 )
  JVS( 580) = W( 61 )
  JVS( 581) = W( 62 )
  JVS( 582) = W( 63 )
  JVS( 583) = W( 64 )
  JVS( 584) = W( 65 )
  JVS( 585) = W( 66 )
  JVS( 586) = W( 70 )
  JVS( 587) = W( 74 )
  JVS( 588) = W( 75 )
  JVS( 589) = W( 76 )
  JVS( 590) = W( 78 )
  JVS( 591) = W( 79 )
  JVS( 592) = W( 80 )
  JVS( 593) = W( 82 )
  JVS( 594) = W( 83 )
  JVS( 595) = W( 84 )
  JVS( 596) = W( 85 )
  IF ( ABS(  JVS( 605 )) < TINY(a) ) THEN
         IER = 75                                      
         RETURN
  END IF
   W( 23 ) = JVS( 597 )
   W( 58 ) = JVS( 598 )
   W( 62 ) = JVS( 599 )
   W( 64 ) = JVS( 600 )
   W( 65 ) = JVS( 601 )
   W( 66 ) = JVS( 602 )
   W( 70 ) = JVS( 603 )
   W( 74 ) = JVS( 604 )
   W( 75 ) = JVS( 605 )
   W( 76 ) = JVS( 606 )
   W( 77 ) = JVS( 607 )
   W( 78 ) = JVS( 608 )
   W( 79 ) = JVS( 609 )
   W( 80 ) = JVS( 610 )
   W( 81 ) = JVS( 611 )
   W( 82 ) = JVS( 612 )
   W( 83 ) = JVS( 613 )
   W( 84 ) = JVS( 614 )
   W( 85 ) = JVS( 615 )
   W( 86 ) = JVS( 616 )
  a = -W( 23 ) / JVS(          114  )
  W( 23 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 115 )
  W( 79 ) = W( 79 ) + a*JVS( 116 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  JVS( 597) = W( 23 )
  JVS( 598) = W( 58 )
  JVS( 599) = W( 62 )
  JVS( 600) = W( 64 )
  JVS( 601) = W( 65 )
  JVS( 602) = W( 66 )
  JVS( 603) = W( 70 )
  JVS( 604) = W( 74 )
  JVS( 605) = W( 75 )
  JVS( 606) = W( 76 )
  JVS( 607) = W( 77 )
  JVS( 608) = W( 78 )
  JVS( 609) = W( 79 )
  JVS( 610) = W( 80 )
  JVS( 611) = W( 81 )
  JVS( 612) = W( 82 )
  JVS( 613) = W( 83 )
  JVS( 614) = W( 84 )
  JVS( 615) = W( 85 )
  JVS( 616) = W( 86 )
  IF ( ABS(  JVS( 640 )) < TINY(a) ) THEN
         IER = 76                                      
         RETURN
  END IF
   W( 20 ) = JVS( 617 )
   W( 27 ) = JVS( 618 )
   W( 34 ) = JVS( 619 )
   W( 39 ) = JVS( 620 )
   W( 41 ) = JVS( 621 )
   W( 42 ) = JVS( 622 )
   W( 50 ) = JVS( 623 )
   W( 54 ) = JVS( 624 )
   W( 59 ) = JVS( 625 )
   W( 61 ) = JVS( 626 )
   W( 62 ) = JVS( 627 )
   W( 63 ) = JVS( 628 )
   W( 64 ) = JVS( 629 )
   W( 65 ) = JVS( 630 )
   W( 66 ) = JVS( 631 )
   W( 67 ) = JVS( 632 )
   W( 69 ) = JVS( 633 )
   W( 70 ) = JVS( 634 )
   W( 71 ) = JVS( 635 )
   W( 72 ) = JVS( 636 )
   W( 73 ) = JVS( 637 )
   W( 74 ) = JVS( 638 )
   W( 75 ) = JVS( 639 )
   W( 76 ) = JVS( 640 )
   W( 77 ) = JVS( 641 )
   W( 78 ) = JVS( 642 )
   W( 79 ) = JVS( 643 )
   W( 80 ) = JVS( 644 )
   W( 81 ) = JVS( 645 )
   W( 82 ) = JVS( 646 )
   W( 83 ) = JVS( 647 )
   W( 84 ) = JVS( 648 )
   W( 85 ) = JVS( 649 )
   W( 86 ) = JVS( 650 )
  a = -W( 20 ) / JVS(          105  )
  W( 20 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 106 )
  W( 79 ) = W( 79 ) + a*JVS( 107 )
  a = -W( 27 ) / JVS(          124  )
  W( 27 ) = -a
  W( 34 ) = W( 34 ) + a*JVS( 125 )
  W( 59 ) = W( 59 ) + a*JVS( 126 )
  W( 61 ) = W( 61 ) + a*JVS( 127 )
  W( 74 ) = W( 74 ) + a*JVS( 128 )
  W( 82 ) = W( 82 ) + a*JVS( 129 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 50 ) / JVS(          222  )
  W( 50 ) = -a
  W( 62 ) = W( 62 ) + a*JVS( 223 )
  W( 64 ) = W( 64 ) + a*JVS( 224 )
  W( 66 ) = W( 66 ) + a*JVS( 225 )
  W( 74 ) = W( 74 ) + a*JVS( 226 )
  W( 80 ) = W( 80 ) + a*JVS( 227 )
  W( 82 ) = W( 82 ) + a*JVS( 228 )
  a = -W( 54 ) / JVS(          285  )
  W( 54 ) = -a
  W( 59 ) = W( 59 ) + a*JVS( 286 )
  W( 61 ) = W( 61 ) + a*JVS( 287 )
  W( 63 ) = W( 63 ) + a*JVS( 288 )
  W( 65 ) = W( 65 ) + a*JVS( 289 )
  W( 69 ) = W( 69 ) + a*JVS( 290 )
  W( 74 ) = W( 74 ) + a*JVS( 291 )
  W( 79 ) = W( 79 ) + a*JVS( 292 )
  W( 80 ) = W( 80 ) + a*JVS( 293 )
  W( 82 ) = W( 82 ) + a*JVS( 294 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 67 ) / JVS(          416  )
  W( 67 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 417 )
  W( 70 ) = W( 70 ) + a*JVS( 418 )
  W( 71 ) = W( 71 ) + a*JVS( 419 )
  W( 72 ) = W( 72 ) + a*JVS( 420 )
  W( 73 ) = W( 73 ) + a*JVS( 421 )
  W( 74 ) = W( 74 ) + a*JVS( 422 )
  W( 75 ) = W( 75 ) + a*JVS( 423 )
  W( 76 ) = W( 76 ) + a*JVS( 424 )
  W( 78 ) = W( 78 ) + a*JVS( 425 )
  W( 80 ) = W( 80 ) + a*JVS( 426 )
  W( 82 ) = W( 82 ) + a*JVS( 427 )
  W( 83 ) = W( 83 ) + a*JVS( 428 )
  W( 84 ) = W( 84 ) + a*JVS( 429 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  JVS( 617) = W( 20 )
  JVS( 618) = W( 27 )
  JVS( 619) = W( 34 )
  JVS( 620) = W( 39 )
  JVS( 621) = W( 41 )
  JVS( 622) = W( 42 )
  JVS( 623) = W( 50 )
  JVS( 624) = W( 54 )
  JVS( 625) = W( 59 )
  JVS( 626) = W( 61 )
  JVS( 627) = W( 62 )
  JVS( 628) = W( 63 )
  JVS( 629) = W( 64 )
  JVS( 630) = W( 65 )
  JVS( 631) = W( 66 )
  JVS( 632) = W( 67 )
  JVS( 633) = W( 69 )
  JVS( 634) = W( 70 )
  JVS( 635) = W( 71 )
  JVS( 636) = W( 72 )
  JVS( 637) = W( 73 )
  JVS( 638) = W( 74 )
  JVS( 639) = W( 75 )
  JVS( 640) = W( 76 )
  JVS( 641) = W( 77 )
  JVS( 642) = W( 78 )
  JVS( 643) = W( 79 )
  JVS( 644) = W( 80 )
  JVS( 645) = W( 81 )
  JVS( 646) = W( 82 )
  JVS( 647) = W( 83 )
  JVS( 648) = W( 84 )
  JVS( 649) = W( 85 )
  JVS( 650) = W( 86 )
  IF ( ABS(  JVS( 685 )) < TINY(a) ) THEN
         IER = 77                                      
         RETURN
  END IF
   W( 19 ) = JVS( 651 )
   W( 25 ) = JVS( 652 )
   W( 26 ) = JVS( 653 )
   W( 30 ) = JVS( 654 )
   W( 32 ) = JVS( 655 )
   W( 34 ) = JVS( 656 )
   W( 39 ) = JVS( 657 )
   W( 40 ) = JVS( 658 )
   W( 41 ) = JVS( 659 )
   W( 42 ) = JVS( 660 )
   W( 43 ) = JVS( 661 )
   W( 44 ) = JVS( 662 )
   W( 45 ) = JVS( 663 )
   W( 48 ) = JVS( 664 )
   W( 49 ) = JVS( 665 )
   W( 53 ) = JVS( 666 )
   W( 55 ) = JVS( 667 )
   W( 57 ) = JVS( 668 )
   W( 58 ) = JVS( 669 )
   W( 59 ) = JVS( 670 )
   W( 61 ) = JVS( 671 )
   W( 62 ) = JVS( 672 )
   W( 63 ) = JVS( 673 )
   W( 64 ) = JVS( 674 )
   W( 65 ) = JVS( 675 )
   W( 66 ) = JVS( 676 )
   W( 69 ) = JVS( 677 )
   W( 70 ) = JVS( 678 )
   W( 71 ) = JVS( 679 )
   W( 72 ) = JVS( 680 )
   W( 73 ) = JVS( 681 )
   W( 74 ) = JVS( 682 )
   W( 75 ) = JVS( 683 )
   W( 76 ) = JVS( 684 )
   W( 77 ) = JVS( 685 )
   W( 78 ) = JVS( 686 )
   W( 79 ) = JVS( 687 )
   W( 80 ) = JVS( 688 )
   W( 81 ) = JVS( 689 )
   W( 82 ) = JVS( 690 )
   W( 83 ) = JVS( 691 )
   W( 84 ) = JVS( 692 )
   W( 85 ) = JVS( 693 )
   W( 86 ) = JVS( 694 )
  a = -W( 19 ) / JVS(          103  )
  W( 19 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 104 )
  a = -W( 25 ) / JVS(          120  )
  W( 25 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 121 )
  a = -W( 26 ) / JVS(          122  )
  W( 26 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 123 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 43 ) / JVS(          182  )
  W( 43 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 183 )
  W( 82 ) = W( 82 ) + a*JVS( 184 )
  a = -W( 44 ) / JVS(          185  )
  W( 44 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 186 )
  W( 82 ) = W( 82 ) + a*JVS( 187 )
  a = -W( 45 ) / JVS(          190  )
  W( 45 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 191 )
  W( 82 ) = W( 82 ) + a*JVS( 192 )
  a = -W( 48 ) / JVS(          206  )
  W( 48 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 207 )
  W( 82 ) = W( 82 ) + a*JVS( 208 )
  W( 85 ) = W( 85 ) + a*JVS( 209 )
  W( 86 ) = W( 86 ) + a*JVS( 210 )
  a = -W( 49 ) / JVS(          212  )
  W( 49 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 213 )
  W( 80 ) = W( 80 ) + a*JVS( 214 )
  W( 82 ) = W( 82 ) + a*JVS( 215 )
  W( 85 ) = W( 85 ) + a*JVS( 216 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 57 ) / JVS(          320  )
  W( 57 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 321 )
  W( 76 ) = W( 76 ) + a*JVS( 322 )
  W( 78 ) = W( 78 ) + a*JVS( 323 )
  W( 79 ) = W( 79 ) + a*JVS( 324 )
  W( 80 ) = W( 80 ) + a*JVS( 325 )
  W( 82 ) = W( 82 ) + a*JVS( 326 )
  W( 83 ) = W( 83 ) + a*JVS( 327 )
  W( 84 ) = W( 84 ) + a*JVS( 328 )
  W( 85 ) = W( 85 ) + a*JVS( 329 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  JVS( 651) = W( 19 )
  JVS( 652) = W( 25 )
  JVS( 653) = W( 26 )
  JVS( 654) = W( 30 )
  JVS( 655) = W( 32 )
  JVS( 656) = W( 34 )
  JVS( 657) = W( 39 )
  JVS( 658) = W( 40 )
  JVS( 659) = W( 41 )
  JVS( 660) = W( 42 )
  JVS( 661) = W( 43 )
  JVS( 662) = W( 44 )
  JVS( 663) = W( 45 )
  JVS( 664) = W( 48 )
  JVS( 665) = W( 49 )
  JVS( 666) = W( 53 )
  JVS( 667) = W( 55 )
  JVS( 668) = W( 57 )
  JVS( 669) = W( 58 )
  JVS( 670) = W( 59 )
  JVS( 671) = W( 61 )
  JVS( 672) = W( 62 )
  JVS( 673) = W( 63 )
  JVS( 674) = W( 64 )
  JVS( 675) = W( 65 )
  JVS( 676) = W( 66 )
  JVS( 677) = W( 69 )
  JVS( 678) = W( 70 )
  JVS( 679) = W( 71 )
  JVS( 680) = W( 72 )
  JVS( 681) = W( 73 )
  JVS( 682) = W( 74 )
  JVS( 683) = W( 75 )
  JVS( 684) = W( 76 )
  JVS( 685) = W( 77 )
  JVS( 686) = W( 78 )
  JVS( 687) = W( 79 )
  JVS( 688) = W( 80 )
  JVS( 689) = W( 81 )
  JVS( 690) = W( 82 )
  JVS( 691) = W( 83 )
  JVS( 692) = W( 84 )
  JVS( 693) = W( 85 )
  JVS( 694) = W( 86 )
  IF ( ABS(  JVS( 711 )) < TINY(a) ) THEN
         IER = 78                                      
         RETURN
  END IF
   W( 29 ) = JVS( 695 )
   W( 36 ) = JVS( 696 )
   W( 60 ) = JVS( 697 )
   W( 61 ) = JVS( 698 )
   W( 63 ) = JVS( 699 )
   W( 65 ) = JVS( 700 )
   W( 66 ) = JVS( 701 )
   W( 68 ) = JVS( 702 )
   W( 69 ) = JVS( 703 )
   W( 70 ) = JVS( 704 )
   W( 72 ) = JVS( 705 )
   W( 73 ) = JVS( 706 )
   W( 74 ) = JVS( 707 )
   W( 75 ) = JVS( 708 )
   W( 76 ) = JVS( 709 )
   W( 77 ) = JVS( 710 )
   W( 78 ) = JVS( 711 )
   W( 79 ) = JVS( 712 )
   W( 80 ) = JVS( 713 )
   W( 81 ) = JVS( 714 )
   W( 82 ) = JVS( 715 )
   W( 83 ) = JVS( 716 )
   W( 84 ) = JVS( 717 )
   W( 85 ) = JVS( 718 )
   W( 86 ) = JVS( 719 )
  a = -W( 29 ) / JVS(          133  )
  W( 29 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 134 )
  W( 82 ) = W( 82 ) + a*JVS( 135 )
  a = -W( 36 ) / JVS(          155  )
  W( 36 ) = -a
  W( 68 ) = W( 68 ) + a*JVS( 156 )
  W( 78 ) = W( 78 ) + a*JVS( 157 )
  W( 85 ) = W( 85 ) + a*JVS( 158 )
  a = -W( 60 ) / JVS(          348  )
  W( 60 ) = -a
  W( 61 ) = W( 61 ) + a*JVS( 349 )
  W( 63 ) = W( 63 ) + a*JVS( 350 )
  W( 65 ) = W( 65 ) + a*JVS( 351 )
  W( 66 ) = W( 66 ) + a*JVS( 352 )
  W( 69 ) = W( 69 ) + a*JVS( 353 )
  W( 70 ) = W( 70 ) + a*JVS( 354 )
  W( 72 ) = W( 72 ) + a*JVS( 355 )
  W( 73 ) = W( 73 ) + a*JVS( 356 )
  W( 74 ) = W( 74 ) + a*JVS( 357 )
  W( 75 ) = W( 75 ) + a*JVS( 358 )
  W( 76 ) = W( 76 ) + a*JVS( 359 )
  W( 77 ) = W( 77 ) + a*JVS( 360 )
  W( 78 ) = W( 78 ) + a*JVS( 361 )
  W( 79 ) = W( 79 ) + a*JVS( 362 )
  W( 80 ) = W( 80 ) + a*JVS( 363 )
  W( 81 ) = W( 81 ) + a*JVS( 364 )
  W( 82 ) = W( 82 ) + a*JVS( 365 )
  W( 83 ) = W( 83 ) + a*JVS( 366 )
  W( 84 ) = W( 84 ) + a*JVS( 367 )
  W( 85 ) = W( 85 ) + a*JVS( 368 )
  W( 86 ) = W( 86 ) + a*JVS( 369 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 68 ) / JVS(          451  )
  W( 68 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 452 )
  W( 70 ) = W( 70 ) + a*JVS( 453 )
  W( 72 ) = W( 72 ) + a*JVS( 454 )
  W( 73 ) = W( 73 ) + a*JVS( 455 )
  W( 74 ) = W( 74 ) + a*JVS( 456 )
  W( 75 ) = W( 75 ) + a*JVS( 457 )
  W( 76 ) = W( 76 ) + a*JVS( 458 )
  W( 77 ) = W( 77 ) + a*JVS( 459 )
  W( 78 ) = W( 78 ) + a*JVS( 460 )
  W( 79 ) = W( 79 ) + a*JVS( 461 )
  W( 80 ) = W( 80 ) + a*JVS( 462 )
  W( 81 ) = W( 81 ) + a*JVS( 463 )
  W( 82 ) = W( 82 ) + a*JVS( 464 )
  W( 83 ) = W( 83 ) + a*JVS( 465 )
  W( 84 ) = W( 84 ) + a*JVS( 466 )
  W( 85 ) = W( 85 ) + a*JVS( 467 )
  W( 86 ) = W( 86 ) + a*JVS( 468 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  JVS( 695) = W( 29 )
  JVS( 696) = W( 36 )
  JVS( 697) = W( 60 )
  JVS( 698) = W( 61 )
  JVS( 699) = W( 63 )
  JVS( 700) = W( 65 )
  JVS( 701) = W( 66 )
  JVS( 702) = W( 68 )
  JVS( 703) = W( 69 )
  JVS( 704) = W( 70 )
  JVS( 705) = W( 72 )
  JVS( 706) = W( 73 )
  JVS( 707) = W( 74 )
  JVS( 708) = W( 75 )
  JVS( 709) = W( 76 )
  JVS( 710) = W( 77 )
  JVS( 711) = W( 78 )
  JVS( 712) = W( 79 )
  JVS( 713) = W( 80 )
  JVS( 714) = W( 81 )
  JVS( 715) = W( 82 )
  JVS( 716) = W( 83 )
  JVS( 717) = W( 84 )
  JVS( 718) = W( 85 )
  JVS( 719) = W( 86 )
  IF ( ABS(  JVS( 755 )) < TINY(a) ) THEN
         IER = 79                                      
         RETURN
  END IF
   W( 20 ) = JVS( 720 )
   W( 21 ) = JVS( 721 )
   W( 22 ) = JVS( 722 )
   W( 23 ) = JVS( 723 )
   W( 28 ) = JVS( 724 )
   W( 29 ) = JVS( 725 )
   W( 31 ) = JVS( 726 )
   W( 36 ) = JVS( 727 )
   W( 37 ) = JVS( 728 )
   W( 38 ) = JVS( 729 )
   W( 46 ) = JVS( 730 )
   W( 52 ) = JVS( 731 )
   W( 56 ) = JVS( 732 )
   W( 57 ) = JVS( 733 )
   W( 58 ) = JVS( 734 )
   W( 59 ) = JVS( 735 )
   W( 60 ) = JVS( 736 )
   W( 61 ) = JVS( 737 )
   W( 62 ) = JVS( 738 )
   W( 63 ) = JVS( 739 )
   W( 64 ) = JVS( 740 )
   W( 65 ) = JVS( 741 )
   W( 66 ) = JVS( 742 )
   W( 67 ) = JVS( 743 )
   W( 68 ) = JVS( 744 )
   W( 69 ) = JVS( 745 )
   W( 70 ) = JVS( 746 )
   W( 71 ) = JVS( 747 )
   W( 72 ) = JVS( 748 )
   W( 73 ) = JVS( 749 )
   W( 74 ) = JVS( 750 )
   W( 75 ) = JVS( 751 )
   W( 76 ) = JVS( 752 )
   W( 77 ) = JVS( 753 )
   W( 78 ) = JVS( 754 )
   W( 79 ) = JVS( 755 )
   W( 80 ) = JVS( 756 )
   W( 81 ) = JVS( 757 )
   W( 82 ) = JVS( 758 )
   W( 83 ) = JVS( 759 )
   W( 84 ) = JVS( 760 )
   W( 85 ) = JVS( 761 )
   W( 86 ) = JVS( 762 )
  a = -W( 20 ) / JVS(          105  )
  W( 20 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 106 )
  W( 79 ) = W( 79 ) + a*JVS( 107 )
  a = -W( 21 ) / JVS(          108  )
  W( 21 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 109 )
  W( 83 ) = W( 83 ) + a*JVS( 110 )
  a = -W( 22 ) / JVS(          111  )
  W( 22 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 112 )
  W( 84 ) = W( 84 ) + a*JVS( 113 )
  a = -W( 23 ) / JVS(          114  )
  W( 23 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 115 )
  W( 79 ) = W( 79 ) + a*JVS( 116 )
  a = -W( 28 ) / JVS(          130  )
  W( 28 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 131 )
  W( 80 ) = W( 80 ) + a*JVS( 132 )
  a = -W( 29 ) / JVS(          133  )
  W( 29 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 134 )
  W( 82 ) = W( 82 ) + a*JVS( 135 )
  a = -W( 31 ) / JVS(          139  )
  W( 31 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 140 )
  W( 82 ) = W( 82 ) + a*JVS( 141 )
  a = -W( 36 ) / JVS(          155  )
  W( 36 ) = -a
  W( 68 ) = W( 68 ) + a*JVS( 156 )
  W( 78 ) = W( 78 ) + a*JVS( 157 )
  W( 85 ) = W( 85 ) + a*JVS( 158 )
  a = -W( 37 ) / JVS(          159  )
  W( 37 ) = -a
  W( 46 ) = W( 46 ) + a*JVS( 160 )
  W( 79 ) = W( 79 ) + a*JVS( 161 )
  W( 80 ) = W( 80 ) + a*JVS( 162 )
  W( 85 ) = W( 85 ) + a*JVS( 163 )
  a = -W( 38 ) / JVS(          164  )
  W( 38 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 165 )
  W( 82 ) = W( 82 ) + a*JVS( 166 )
  W( 85 ) = W( 85 ) + a*JVS( 167 )
  a = -W( 46 ) / JVS(          194  )
  W( 46 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 195 )
  W( 79 ) = W( 79 ) + a*JVS( 196 )
  W( 80 ) = W( 80 ) + a*JVS( 197 )
  W( 85 ) = W( 85 ) + a*JVS( 198 )
  a = -W( 52 ) / JVS(          260  )
  W( 52 ) = -a
  W( 56 ) = W( 56 ) + a*JVS( 261 )
  W( 57 ) = W( 57 ) + a*JVS( 262 )
  W( 62 ) = W( 62 ) + a*JVS( 263 )
  W( 64 ) = W( 64 ) + a*JVS( 264 )
  W( 65 ) = W( 65 ) + a*JVS( 265 )
  W( 66 ) = W( 66 ) + a*JVS( 266 )
  W( 67 ) = W( 67 ) + a*JVS( 267 )
  W( 68 ) = W( 68 ) + a*JVS( 268 )
  W( 71 ) = W( 71 ) + a*JVS( 269 )
  W( 74 ) = W( 74 ) + a*JVS( 270 )
  W( 79 ) = W( 79 ) + a*JVS( 271 )
  W( 80 ) = W( 80 ) + a*JVS( 272 )
  W( 82 ) = W( 82 ) + a*JVS( 273 )
  W( 85 ) = W( 85 ) + a*JVS( 274 )
  a = -W( 56 ) / JVS(          308  )
  W( 56 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 309 )
  W( 59 ) = W( 59 ) + a*JVS( 310 )
  W( 61 ) = W( 61 ) + a*JVS( 311 )
  W( 64 ) = W( 64 ) + a*JVS( 312 )
  W( 70 ) = W( 70 ) + a*JVS( 313 )
  W( 74 ) = W( 74 ) + a*JVS( 314 )
  W( 80 ) = W( 80 ) + a*JVS( 315 )
  W( 82 ) = W( 82 ) + a*JVS( 316 )
  W( 85 ) = W( 85 ) + a*JVS( 317 )
  a = -W( 57 ) / JVS(          320  )
  W( 57 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 321 )
  W( 76 ) = W( 76 ) + a*JVS( 322 )
  W( 78 ) = W( 78 ) + a*JVS( 323 )
  W( 79 ) = W( 79 ) + a*JVS( 324 )
  W( 80 ) = W( 80 ) + a*JVS( 325 )
  W( 82 ) = W( 82 ) + a*JVS( 326 )
  W( 83 ) = W( 83 ) + a*JVS( 327 )
  W( 84 ) = W( 84 ) + a*JVS( 328 )
  W( 85 ) = W( 85 ) + a*JVS( 329 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 60 ) / JVS(          348  )
  W( 60 ) = -a
  W( 61 ) = W( 61 ) + a*JVS( 349 )
  W( 63 ) = W( 63 ) + a*JVS( 350 )
  W( 65 ) = W( 65 ) + a*JVS( 351 )
  W( 66 ) = W( 66 ) + a*JVS( 352 )
  W( 69 ) = W( 69 ) + a*JVS( 353 )
  W( 70 ) = W( 70 ) + a*JVS( 354 )
  W( 72 ) = W( 72 ) + a*JVS( 355 )
  W( 73 ) = W( 73 ) + a*JVS( 356 )
  W( 74 ) = W( 74 ) + a*JVS( 357 )
  W( 75 ) = W( 75 ) + a*JVS( 358 )
  W( 76 ) = W( 76 ) + a*JVS( 359 )
  W( 77 ) = W( 77 ) + a*JVS( 360 )
  W( 78 ) = W( 78 ) + a*JVS( 361 )
  W( 79 ) = W( 79 ) + a*JVS( 362 )
  W( 80 ) = W( 80 ) + a*JVS( 363 )
  W( 81 ) = W( 81 ) + a*JVS( 364 )
  W( 82 ) = W( 82 ) + a*JVS( 365 )
  W( 83 ) = W( 83 ) + a*JVS( 366 )
  W( 84 ) = W( 84 ) + a*JVS( 367 )
  W( 85 ) = W( 85 ) + a*JVS( 368 )
  W( 86 ) = W( 86 ) + a*JVS( 369 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 67 ) / JVS(          416  )
  W( 67 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 417 )
  W( 70 ) = W( 70 ) + a*JVS( 418 )
  W( 71 ) = W( 71 ) + a*JVS( 419 )
  W( 72 ) = W( 72 ) + a*JVS( 420 )
  W( 73 ) = W( 73 ) + a*JVS( 421 )
  W( 74 ) = W( 74 ) + a*JVS( 422 )
  W( 75 ) = W( 75 ) + a*JVS( 423 )
  W( 76 ) = W( 76 ) + a*JVS( 424 )
  W( 78 ) = W( 78 ) + a*JVS( 425 )
  W( 80 ) = W( 80 ) + a*JVS( 426 )
  W( 82 ) = W( 82 ) + a*JVS( 427 )
  W( 83 ) = W( 83 ) + a*JVS( 428 )
  W( 84 ) = W( 84 ) + a*JVS( 429 )
  a = -W( 68 ) / JVS(          451  )
  W( 68 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 452 )
  W( 70 ) = W( 70 ) + a*JVS( 453 )
  W( 72 ) = W( 72 ) + a*JVS( 454 )
  W( 73 ) = W( 73 ) + a*JVS( 455 )
  W( 74 ) = W( 74 ) + a*JVS( 456 )
  W( 75 ) = W( 75 ) + a*JVS( 457 )
  W( 76 ) = W( 76 ) + a*JVS( 458 )
  W( 77 ) = W( 77 ) + a*JVS( 459 )
  W( 78 ) = W( 78 ) + a*JVS( 460 )
  W( 79 ) = W( 79 ) + a*JVS( 461 )
  W( 80 ) = W( 80 ) + a*JVS( 462 )
  W( 81 ) = W( 81 ) + a*JVS( 463 )
  W( 82 ) = W( 82 ) + a*JVS( 464 )
  W( 83 ) = W( 83 ) + a*JVS( 465 )
  W( 84 ) = W( 84 ) + a*JVS( 466 )
  W( 85 ) = W( 85 ) + a*JVS( 467 )
  W( 86 ) = W( 86 ) + a*JVS( 468 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  JVS( 720) = W( 20 )
  JVS( 721) = W( 21 )
  JVS( 722) = W( 22 )
  JVS( 723) = W( 23 )
  JVS( 724) = W( 28 )
  JVS( 725) = W( 29 )
  JVS( 726) = W( 31 )
  JVS( 727) = W( 36 )
  JVS( 728) = W( 37 )
  JVS( 729) = W( 38 )
  JVS( 730) = W( 46 )
  JVS( 731) = W( 52 )
  JVS( 732) = W( 56 )
  JVS( 733) = W( 57 )
  JVS( 734) = W( 58 )
  JVS( 735) = W( 59 )
  JVS( 736) = W( 60 )
  JVS( 737) = W( 61 )
  JVS( 738) = W( 62 )
  JVS( 739) = W( 63 )
  JVS( 740) = W( 64 )
  JVS( 741) = W( 65 )
  JVS( 742) = W( 66 )
  JVS( 743) = W( 67 )
  JVS( 744) = W( 68 )
  JVS( 745) = W( 69 )
  JVS( 746) = W( 70 )
  JVS( 747) = W( 71 )
  JVS( 748) = W( 72 )
  JVS( 749) = W( 73 )
  JVS( 750) = W( 74 )
  JVS( 751) = W( 75 )
  JVS( 752) = W( 76 )
  JVS( 753) = W( 77 )
  JVS( 754) = W( 78 )
  JVS( 755) = W( 79 )
  JVS( 756) = W( 80 )
  JVS( 757) = W( 81 )
  JVS( 758) = W( 82 )
  JVS( 759) = W( 83 )
  JVS( 760) = W( 84 )
  JVS( 761) = W( 85 )
  JVS( 762) = W( 86 )
  IF ( ABS(  JVS( 797 )) < TINY(a) ) THEN
         IER = 80                                      
         RETURN
  END IF
   W( 28 ) = JVS( 763 )
   W( 38 ) = JVS( 764 )
   W( 43 ) = JVS( 765 )
   W( 46 ) = JVS( 766 )
   W( 47 ) = JVS( 767 )
   W( 49 ) = JVS( 768 )
   W( 50 ) = JVS( 769 )
   W( 52 ) = JVS( 770 )
   W( 53 ) = JVS( 771 )
   W( 55 ) = JVS( 772 )
   W( 56 ) = JVS( 773 )
   W( 57 ) = JVS( 774 )
   W( 58 ) = JVS( 775 )
   W( 59 ) = JVS( 776 )
   W( 60 ) = JVS( 777 )
   W( 61 ) = JVS( 778 )
   W( 62 ) = JVS( 779 )
   W( 63 ) = JVS( 780 )
   W( 64 ) = JVS( 781 )
   W( 65 ) = JVS( 782 )
   W( 66 ) = JVS( 783 )
   W( 67 ) = JVS( 784 )
   W( 68 ) = JVS( 785 )
   W( 69 ) = JVS( 786 )
   W( 70 ) = JVS( 787 )
   W( 71 ) = JVS( 788 )
   W( 72 ) = JVS( 789 )
   W( 73 ) = JVS( 790 )
   W( 74 ) = JVS( 791 )
   W( 75 ) = JVS( 792 )
   W( 76 ) = JVS( 793 )
   W( 77 ) = JVS( 794 )
   W( 78 ) = JVS( 795 )
   W( 79 ) = JVS( 796 )
   W( 80 ) = JVS( 797 )
   W( 81 ) = JVS( 798 )
   W( 82 ) = JVS( 799 )
   W( 83 ) = JVS( 800 )
   W( 84 ) = JVS( 801 )
   W( 85 ) = JVS( 802 )
   W( 86 ) = JVS( 803 )
  a = -W( 28 ) / JVS(          130  )
  W( 28 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 131 )
  W( 80 ) = W( 80 ) + a*JVS( 132 )
  a = -W( 38 ) / JVS(          164  )
  W( 38 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 165 )
  W( 82 ) = W( 82 ) + a*JVS( 166 )
  W( 85 ) = W( 85 ) + a*JVS( 167 )
  a = -W( 43 ) / JVS(          182  )
  W( 43 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 183 )
  W( 82 ) = W( 82 ) + a*JVS( 184 )
  a = -W( 46 ) / JVS(          194  )
  W( 46 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 195 )
  W( 79 ) = W( 79 ) + a*JVS( 196 )
  W( 80 ) = W( 80 ) + a*JVS( 197 )
  W( 85 ) = W( 85 ) + a*JVS( 198 )
  a = -W( 47 ) / JVS(          201  )
  W( 47 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 202 )
  W( 74 ) = W( 74 ) + a*JVS( 203 )
  W( 80 ) = W( 80 ) + a*JVS( 204 )
  W( 82 ) = W( 82 ) + a*JVS( 205 )
  a = -W( 49 ) / JVS(          212  )
  W( 49 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 213 )
  W( 80 ) = W( 80 ) + a*JVS( 214 )
  W( 82 ) = W( 82 ) + a*JVS( 215 )
  W( 85 ) = W( 85 ) + a*JVS( 216 )
  a = -W( 50 ) / JVS(          222  )
  W( 50 ) = -a
  W( 62 ) = W( 62 ) + a*JVS( 223 )
  W( 64 ) = W( 64 ) + a*JVS( 224 )
  W( 66 ) = W( 66 ) + a*JVS( 225 )
  W( 74 ) = W( 74 ) + a*JVS( 226 )
  W( 80 ) = W( 80 ) + a*JVS( 227 )
  W( 82 ) = W( 82 ) + a*JVS( 228 )
  a = -W( 52 ) / JVS(          260  )
  W( 52 ) = -a
  W( 56 ) = W( 56 ) + a*JVS( 261 )
  W( 57 ) = W( 57 ) + a*JVS( 262 )
  W( 62 ) = W( 62 ) + a*JVS( 263 )
  W( 64 ) = W( 64 ) + a*JVS( 264 )
  W( 65 ) = W( 65 ) + a*JVS( 265 )
  W( 66 ) = W( 66 ) + a*JVS( 266 )
  W( 67 ) = W( 67 ) + a*JVS( 267 )
  W( 68 ) = W( 68 ) + a*JVS( 268 )
  W( 71 ) = W( 71 ) + a*JVS( 269 )
  W( 74 ) = W( 74 ) + a*JVS( 270 )
  W( 79 ) = W( 79 ) + a*JVS( 271 )
  W( 80 ) = W( 80 ) + a*JVS( 272 )
  W( 82 ) = W( 82 ) + a*JVS( 273 )
  W( 85 ) = W( 85 ) + a*JVS( 274 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 56 ) / JVS(          308  )
  W( 56 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 309 )
  W( 59 ) = W( 59 ) + a*JVS( 310 )
  W( 61 ) = W( 61 ) + a*JVS( 311 )
  W( 64 ) = W( 64 ) + a*JVS( 312 )
  W( 70 ) = W( 70 ) + a*JVS( 313 )
  W( 74 ) = W( 74 ) + a*JVS( 314 )
  W( 80 ) = W( 80 ) + a*JVS( 315 )
  W( 82 ) = W( 82 ) + a*JVS( 316 )
  W( 85 ) = W( 85 ) + a*JVS( 317 )
  a = -W( 57 ) / JVS(          320  )
  W( 57 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 321 )
  W( 76 ) = W( 76 ) + a*JVS( 322 )
  W( 78 ) = W( 78 ) + a*JVS( 323 )
  W( 79 ) = W( 79 ) + a*JVS( 324 )
  W( 80 ) = W( 80 ) + a*JVS( 325 )
  W( 82 ) = W( 82 ) + a*JVS( 326 )
  W( 83 ) = W( 83 ) + a*JVS( 327 )
  W( 84 ) = W( 84 ) + a*JVS( 328 )
  W( 85 ) = W( 85 ) + a*JVS( 329 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 60 ) / JVS(          348  )
  W( 60 ) = -a
  W( 61 ) = W( 61 ) + a*JVS( 349 )
  W( 63 ) = W( 63 ) + a*JVS( 350 )
  W( 65 ) = W( 65 ) + a*JVS( 351 )
  W( 66 ) = W( 66 ) + a*JVS( 352 )
  W( 69 ) = W( 69 ) + a*JVS( 353 )
  W( 70 ) = W( 70 ) + a*JVS( 354 )
  W( 72 ) = W( 72 ) + a*JVS( 355 )
  W( 73 ) = W( 73 ) + a*JVS( 356 )
  W( 74 ) = W( 74 ) + a*JVS( 357 )
  W( 75 ) = W( 75 ) + a*JVS( 358 )
  W( 76 ) = W( 76 ) + a*JVS( 359 )
  W( 77 ) = W( 77 ) + a*JVS( 360 )
  W( 78 ) = W( 78 ) + a*JVS( 361 )
  W( 79 ) = W( 79 ) + a*JVS( 362 )
  W( 80 ) = W( 80 ) + a*JVS( 363 )
  W( 81 ) = W( 81 ) + a*JVS( 364 )
  W( 82 ) = W( 82 ) + a*JVS( 365 )
  W( 83 ) = W( 83 ) + a*JVS( 366 )
  W( 84 ) = W( 84 ) + a*JVS( 367 )
  W( 85 ) = W( 85 ) + a*JVS( 368 )
  W( 86 ) = W( 86 ) + a*JVS( 369 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 67 ) / JVS(          416  )
  W( 67 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 417 )
  W( 70 ) = W( 70 ) + a*JVS( 418 )
  W( 71 ) = W( 71 ) + a*JVS( 419 )
  W( 72 ) = W( 72 ) + a*JVS( 420 )
  W( 73 ) = W( 73 ) + a*JVS( 421 )
  W( 74 ) = W( 74 ) + a*JVS( 422 )
  W( 75 ) = W( 75 ) + a*JVS( 423 )
  W( 76 ) = W( 76 ) + a*JVS( 424 )
  W( 78 ) = W( 78 ) + a*JVS( 425 )
  W( 80 ) = W( 80 ) + a*JVS( 426 )
  W( 82 ) = W( 82 ) + a*JVS( 427 )
  W( 83 ) = W( 83 ) + a*JVS( 428 )
  W( 84 ) = W( 84 ) + a*JVS( 429 )
  a = -W( 68 ) / JVS(          451  )
  W( 68 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 452 )
  W( 70 ) = W( 70 ) + a*JVS( 453 )
  W( 72 ) = W( 72 ) + a*JVS( 454 )
  W( 73 ) = W( 73 ) + a*JVS( 455 )
  W( 74 ) = W( 74 ) + a*JVS( 456 )
  W( 75 ) = W( 75 ) + a*JVS( 457 )
  W( 76 ) = W( 76 ) + a*JVS( 458 )
  W( 77 ) = W( 77 ) + a*JVS( 459 )
  W( 78 ) = W( 78 ) + a*JVS( 460 )
  W( 79 ) = W( 79 ) + a*JVS( 461 )
  W( 80 ) = W( 80 ) + a*JVS( 462 )
  W( 81 ) = W( 81 ) + a*JVS( 463 )
  W( 82 ) = W( 82 ) + a*JVS( 464 )
  W( 83 ) = W( 83 ) + a*JVS( 465 )
  W( 84 ) = W( 84 ) + a*JVS( 466 )
  W( 85 ) = W( 85 ) + a*JVS( 467 )
  W( 86 ) = W( 86 ) + a*JVS( 468 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  a = -W( 79 ) / JVS(          755  )
  W( 79 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 756 )
  W( 81 ) = W( 81 ) + a*JVS( 757 )
  W( 82 ) = W( 82 ) + a*JVS( 758 )
  W( 83 ) = W( 83 ) + a*JVS( 759 )
  W( 84 ) = W( 84 ) + a*JVS( 760 )
  W( 85 ) = W( 85 ) + a*JVS( 761 )
  W( 86 ) = W( 86 ) + a*JVS( 762 )
  JVS( 763) = W( 28 )
  JVS( 764) = W( 38 )
  JVS( 765) = W( 43 )
  JVS( 766) = W( 46 )
  JVS( 767) = W( 47 )
  JVS( 768) = W( 49 )
  JVS( 769) = W( 50 )
  JVS( 770) = W( 52 )
  JVS( 771) = W( 53 )
  JVS( 772) = W( 55 )
  JVS( 773) = W( 56 )
  JVS( 774) = W( 57 )
  JVS( 775) = W( 58 )
  JVS( 776) = W( 59 )
  JVS( 777) = W( 60 )
  JVS( 778) = W( 61 )
  JVS( 779) = W( 62 )
  JVS( 780) = W( 63 )
  JVS( 781) = W( 64 )
  JVS( 782) = W( 65 )
  JVS( 783) = W( 66 )
  JVS( 784) = W( 67 )
  JVS( 785) = W( 68 )
  JVS( 786) = W( 69 )
  JVS( 787) = W( 70 )
  JVS( 788) = W( 71 )
  JVS( 789) = W( 72 )
  JVS( 790) = W( 73 )
  JVS( 791) = W( 74 )
  JVS( 792) = W( 75 )
  JVS( 793) = W( 76 )
  JVS( 794) = W( 77 )
  JVS( 795) = W( 78 )
  JVS( 796) = W( 79 )
  JVS( 797) = W( 80 )
  JVS( 798) = W( 81 )
  JVS( 799) = W( 82 )
  JVS( 800) = W( 83 )
  JVS( 801) = W( 84 )
  JVS( 802) = W( 85 )
  JVS( 803) = W( 86 )
  IF ( ABS(  JVS( 830 )) < TINY(a) ) THEN
         IER = 81                                      
         RETURN
  END IF
   W( 15 ) = JVS( 804 )
   W( 31 ) = JVS( 805 )
   W( 35 ) = JVS( 806 )
   W( 39 ) = JVS( 807 )
   W( 53 ) = JVS( 808 )
   W( 54 ) = JVS( 809 )
   W( 55 ) = JVS( 810 )
   W( 58 ) = JVS( 811 )
   W( 59 ) = JVS( 812 )
   W( 61 ) = JVS( 813 )
   W( 63 ) = JVS( 814 )
   W( 65 ) = JVS( 815 )
   W( 66 ) = JVS( 816 )
   W( 67 ) = JVS( 817 )
   W( 69 ) = JVS( 818 )
   W( 70 ) = JVS( 819 )
   W( 71 ) = JVS( 820 )
   W( 72 ) = JVS( 821 )
   W( 73 ) = JVS( 822 )
   W( 74 ) = JVS( 823 )
   W( 75 ) = JVS( 824 )
   W( 76 ) = JVS( 825 )
   W( 77 ) = JVS( 826 )
   W( 78 ) = JVS( 827 )
   W( 79 ) = JVS( 828 )
   W( 80 ) = JVS( 829 )
   W( 81 ) = JVS( 830 )
   W( 82 ) = JVS( 831 )
   W( 83 ) = JVS( 832 )
   W( 84 ) = JVS( 833 )
   W( 85 ) = JVS( 834 )
   W( 86 ) = JVS( 835 )
  a = -W( 15 ) / JVS(           89  )
  W( 15 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 90 )
  a = -W( 31 ) / JVS(          139  )
  W( 31 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 140 )
  W( 82 ) = W( 82 ) + a*JVS( 141 )
  a = -W( 35 ) / JVS(          151  )
  W( 35 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 152 )
  W( 82 ) = W( 82 ) + a*JVS( 153 )
  W( 85 ) = W( 85 ) + a*JVS( 154 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 54 ) / JVS(          285  )
  W( 54 ) = -a
  W( 59 ) = W( 59 ) + a*JVS( 286 )
  W( 61 ) = W( 61 ) + a*JVS( 287 )
  W( 63 ) = W( 63 ) + a*JVS( 288 )
  W( 65 ) = W( 65 ) + a*JVS( 289 )
  W( 69 ) = W( 69 ) + a*JVS( 290 )
  W( 74 ) = W( 74 ) + a*JVS( 291 )
  W( 79 ) = W( 79 ) + a*JVS( 292 )
  W( 80 ) = W( 80 ) + a*JVS( 293 )
  W( 82 ) = W( 82 ) + a*JVS( 294 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 67 ) / JVS(          416  )
  W( 67 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 417 )
  W( 70 ) = W( 70 ) + a*JVS( 418 )
  W( 71 ) = W( 71 ) + a*JVS( 419 )
  W( 72 ) = W( 72 ) + a*JVS( 420 )
  W( 73 ) = W( 73 ) + a*JVS( 421 )
  W( 74 ) = W( 74 ) + a*JVS( 422 )
  W( 75 ) = W( 75 ) + a*JVS( 423 )
  W( 76 ) = W( 76 ) + a*JVS( 424 )
  W( 78 ) = W( 78 ) + a*JVS( 425 )
  W( 80 ) = W( 80 ) + a*JVS( 426 )
  W( 82 ) = W( 82 ) + a*JVS( 427 )
  W( 83 ) = W( 83 ) + a*JVS( 428 )
  W( 84 ) = W( 84 ) + a*JVS( 429 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  a = -W( 79 ) / JVS(          755  )
  W( 79 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 756 )
  W( 81 ) = W( 81 ) + a*JVS( 757 )
  W( 82 ) = W( 82 ) + a*JVS( 758 )
  W( 83 ) = W( 83 ) + a*JVS( 759 )
  W( 84 ) = W( 84 ) + a*JVS( 760 )
  W( 85 ) = W( 85 ) + a*JVS( 761 )
  W( 86 ) = W( 86 ) + a*JVS( 762 )
  a = -W( 80 ) / JVS(          797  )
  W( 80 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 798 )
  W( 82 ) = W( 82 ) + a*JVS( 799 )
  W( 83 ) = W( 83 ) + a*JVS( 800 )
  W( 84 ) = W( 84 ) + a*JVS( 801 )
  W( 85 ) = W( 85 ) + a*JVS( 802 )
  W( 86 ) = W( 86 ) + a*JVS( 803 )
  JVS( 804) = W( 15 )
  JVS( 805) = W( 31 )
  JVS( 806) = W( 35 )
  JVS( 807) = W( 39 )
  JVS( 808) = W( 53 )
  JVS( 809) = W( 54 )
  JVS( 810) = W( 55 )
  JVS( 811) = W( 58 )
  JVS( 812) = W( 59 )
  JVS( 813) = W( 61 )
  JVS( 814) = W( 63 )
  JVS( 815) = W( 65 )
  JVS( 816) = W( 66 )
  JVS( 817) = W( 67 )
  JVS( 818) = W( 69 )
  JVS( 819) = W( 70 )
  JVS( 820) = W( 71 )
  JVS( 821) = W( 72 )
  JVS( 822) = W( 73 )
  JVS( 823) = W( 74 )
  JVS( 824) = W( 75 )
  JVS( 825) = W( 76 )
  JVS( 826) = W( 77 )
  JVS( 827) = W( 78 )
  JVS( 828) = W( 79 )
  JVS( 829) = W( 80 )
  JVS( 830) = W( 81 )
  JVS( 831) = W( 82 )
  JVS( 832) = W( 83 )
  JVS( 833) = W( 84 )
  JVS( 834) = W( 85 )
  JVS( 835) = W( 86 )
  IF ( ABS(  JVS( 891 )) < TINY(a) ) THEN
         IER = 82                                      
         RETURN
  END IF
   W( 14 ) = JVS( 836 )
   W( 15 ) = JVS( 837 )
   W( 16 ) = JVS( 838 )
   W( 19 ) = JVS( 839 )
   W( 24 ) = JVS( 840 )
   W( 25 ) = JVS( 841 )
   W( 26 ) = JVS( 842 )
   W( 29 ) = JVS( 843 )
   W( 30 ) = JVS( 844 )
   W( 32 ) = JVS( 845 )
   W( 33 ) = JVS( 846 )
   W( 34 ) = JVS( 847 )
   W( 35 ) = JVS( 848 )
   W( 38 ) = JVS( 849 )
   W( 39 ) = JVS( 850 )
   W( 40 ) = JVS( 851 )
   W( 41 ) = JVS( 852 )
   W( 42 ) = JVS( 853 )
   W( 43 ) = JVS( 854 )
   W( 44 ) = JVS( 855 )
   W( 45 ) = JVS( 856 )
   W( 47 ) = JVS( 857 )
   W( 48 ) = JVS( 858 )
   W( 49 ) = JVS( 859 )
   W( 50 ) = JVS( 860 )
   W( 51 ) = JVS( 861 )
   W( 52 ) = JVS( 862 )
   W( 53 ) = JVS( 863 )
   W( 54 ) = JVS( 864 )
   W( 55 ) = JVS( 865 )
   W( 56 ) = JVS( 866 )
   W( 57 ) = JVS( 867 )
   W( 58 ) = JVS( 868 )
   W( 59 ) = JVS( 869 )
   W( 61 ) = JVS( 870 )
   W( 62 ) = JVS( 871 )
   W( 63 ) = JVS( 872 )
   W( 64 ) = JVS( 873 )
   W( 65 ) = JVS( 874 )
   W( 66 ) = JVS( 875 )
   W( 67 ) = JVS( 876 )
   W( 68 ) = JVS( 877 )
   W( 69 ) = JVS( 878 )
   W( 70 ) = JVS( 879 )
   W( 71 ) = JVS( 880 )
   W( 72 ) = JVS( 881 )
   W( 73 ) = JVS( 882 )
   W( 74 ) = JVS( 883 )
   W( 75 ) = JVS( 884 )
   W( 76 ) = JVS( 885 )
   W( 77 ) = JVS( 886 )
   W( 78 ) = JVS( 887 )
   W( 79 ) = JVS( 888 )
   W( 80 ) = JVS( 889 )
   W( 81 ) = JVS( 890 )
   W( 82 ) = JVS( 891 )
   W( 83 ) = JVS( 892 )
   W( 84 ) = JVS( 893 )
   W( 85 ) = JVS( 894 )
   W( 86 ) = JVS( 895 )
  a = -W( 14 ) / JVS(           87  )
  W( 14 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 88 )
  a = -W( 15 ) / JVS(           89  )
  W( 15 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 90 )
  a = -W( 16 ) / JVS(           91  )
  W( 16 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 92 )
  a = -W( 19 ) / JVS(          103  )
  W( 19 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 104 )
  a = -W( 24 ) / JVS(          117  )
  W( 24 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 118 )
  W( 85 ) = W( 85 ) + a*JVS( 119 )
  a = -W( 25 ) / JVS(          120  )
  W( 25 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 121 )
  a = -W( 26 ) / JVS(          122  )
  W( 26 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 123 )
  a = -W( 29 ) / JVS(          133  )
  W( 29 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 134 )
  W( 82 ) = W( 82 ) + a*JVS( 135 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 33 ) / JVS(          144  )
  W( 33 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 145 )
  W( 81 ) = W( 81 ) + a*JVS( 146 )
  W( 82 ) = W( 82 ) + a*JVS( 147 )
  W( 86 ) = W( 86 ) + a*JVS( 148 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 35 ) / JVS(          151  )
  W( 35 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 152 )
  W( 82 ) = W( 82 ) + a*JVS( 153 )
  W( 85 ) = W( 85 ) + a*JVS( 154 )
  a = -W( 38 ) / JVS(          164  )
  W( 38 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 165 )
  W( 82 ) = W( 82 ) + a*JVS( 166 )
  W( 85 ) = W( 85 ) + a*JVS( 167 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 43 ) / JVS(          182  )
  W( 43 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 183 )
  W( 82 ) = W( 82 ) + a*JVS( 184 )
  a = -W( 44 ) / JVS(          185  )
  W( 44 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 186 )
  W( 82 ) = W( 82 ) + a*JVS( 187 )
  a = -W( 45 ) / JVS(          190  )
  W( 45 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 191 )
  W( 82 ) = W( 82 ) + a*JVS( 192 )
  a = -W( 47 ) / JVS(          201  )
  W( 47 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 202 )
  W( 74 ) = W( 74 ) + a*JVS( 203 )
  W( 80 ) = W( 80 ) + a*JVS( 204 )
  W( 82 ) = W( 82 ) + a*JVS( 205 )
  a = -W( 48 ) / JVS(          206  )
  W( 48 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 207 )
  W( 82 ) = W( 82 ) + a*JVS( 208 )
  W( 85 ) = W( 85 ) + a*JVS( 209 )
  W( 86 ) = W( 86 ) + a*JVS( 210 )
  a = -W( 49 ) / JVS(          212  )
  W( 49 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 213 )
  W( 80 ) = W( 80 ) + a*JVS( 214 )
  W( 82 ) = W( 82 ) + a*JVS( 215 )
  W( 85 ) = W( 85 ) + a*JVS( 216 )
  a = -W( 50 ) / JVS(          222  )
  W( 50 ) = -a
  W( 62 ) = W( 62 ) + a*JVS( 223 )
  W( 64 ) = W( 64 ) + a*JVS( 224 )
  W( 66 ) = W( 66 ) + a*JVS( 225 )
  W( 74 ) = W( 74 ) + a*JVS( 226 )
  W( 80 ) = W( 80 ) + a*JVS( 227 )
  W( 82 ) = W( 82 ) + a*JVS( 228 )
  a = -W( 51 ) / JVS(          235  )
  W( 51 ) = -a
  W( 53 ) = W( 53 ) + a*JVS( 236 )
  W( 55 ) = W( 55 ) + a*JVS( 237 )
  W( 56 ) = W( 56 ) + a*JVS( 238 )
  W( 58 ) = W( 58 ) + a*JVS( 239 )
  W( 59 ) = W( 59 ) + a*JVS( 240 )
  W( 61 ) = W( 61 ) + a*JVS( 241 )
  W( 62 ) = W( 62 ) + a*JVS( 242 )
  W( 63 ) = W( 63 ) + a*JVS( 243 )
  W( 64 ) = W( 64 ) + a*JVS( 244 )
  W( 65 ) = W( 65 ) + a*JVS( 245 )
  W( 66 ) = W( 66 ) + a*JVS( 246 )
  W( 67 ) = W( 67 ) + a*JVS( 247 )
  W( 68 ) = W( 68 ) + a*JVS( 248 )
  W( 70 ) = W( 70 ) + a*JVS( 249 )
  W( 71 ) = W( 71 ) + a*JVS( 250 )
  W( 74 ) = W( 74 ) + a*JVS( 251 )
  W( 80 ) = W( 80 ) + a*JVS( 252 )
  W( 82 ) = W( 82 ) + a*JVS( 253 )
  a = -W( 52 ) / JVS(          260  )
  W( 52 ) = -a
  W( 56 ) = W( 56 ) + a*JVS( 261 )
  W( 57 ) = W( 57 ) + a*JVS( 262 )
  W( 62 ) = W( 62 ) + a*JVS( 263 )
  W( 64 ) = W( 64 ) + a*JVS( 264 )
  W( 65 ) = W( 65 ) + a*JVS( 265 )
  W( 66 ) = W( 66 ) + a*JVS( 266 )
  W( 67 ) = W( 67 ) + a*JVS( 267 )
  W( 68 ) = W( 68 ) + a*JVS( 268 )
  W( 71 ) = W( 71 ) + a*JVS( 269 )
  W( 74 ) = W( 74 ) + a*JVS( 270 )
  W( 79 ) = W( 79 ) + a*JVS( 271 )
  W( 80 ) = W( 80 ) + a*JVS( 272 )
  W( 82 ) = W( 82 ) + a*JVS( 273 )
  W( 85 ) = W( 85 ) + a*JVS( 274 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 54 ) / JVS(          285  )
  W( 54 ) = -a
  W( 59 ) = W( 59 ) + a*JVS( 286 )
  W( 61 ) = W( 61 ) + a*JVS( 287 )
  W( 63 ) = W( 63 ) + a*JVS( 288 )
  W( 65 ) = W( 65 ) + a*JVS( 289 )
  W( 69 ) = W( 69 ) + a*JVS( 290 )
  W( 74 ) = W( 74 ) + a*JVS( 291 )
  W( 79 ) = W( 79 ) + a*JVS( 292 )
  W( 80 ) = W( 80 ) + a*JVS( 293 )
  W( 82 ) = W( 82 ) + a*JVS( 294 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 56 ) / JVS(          308  )
  W( 56 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 309 )
  W( 59 ) = W( 59 ) + a*JVS( 310 )
  W( 61 ) = W( 61 ) + a*JVS( 311 )
  W( 64 ) = W( 64 ) + a*JVS( 312 )
  W( 70 ) = W( 70 ) + a*JVS( 313 )
  W( 74 ) = W( 74 ) + a*JVS( 314 )
  W( 80 ) = W( 80 ) + a*JVS( 315 )
  W( 82 ) = W( 82 ) + a*JVS( 316 )
  W( 85 ) = W( 85 ) + a*JVS( 317 )
  a = -W( 57 ) / JVS(          320  )
  W( 57 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 321 )
  W( 76 ) = W( 76 ) + a*JVS( 322 )
  W( 78 ) = W( 78 ) + a*JVS( 323 )
  W( 79 ) = W( 79 ) + a*JVS( 324 )
  W( 80 ) = W( 80 ) + a*JVS( 325 )
  W( 82 ) = W( 82 ) + a*JVS( 326 )
  W( 83 ) = W( 83 ) + a*JVS( 327 )
  W( 84 ) = W( 84 ) + a*JVS( 328 )
  W( 85 ) = W( 85 ) + a*JVS( 329 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 67 ) / JVS(          416  )
  W( 67 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 417 )
  W( 70 ) = W( 70 ) + a*JVS( 418 )
  W( 71 ) = W( 71 ) + a*JVS( 419 )
  W( 72 ) = W( 72 ) + a*JVS( 420 )
  W( 73 ) = W( 73 ) + a*JVS( 421 )
  W( 74 ) = W( 74 ) + a*JVS( 422 )
  W( 75 ) = W( 75 ) + a*JVS( 423 )
  W( 76 ) = W( 76 ) + a*JVS( 424 )
  W( 78 ) = W( 78 ) + a*JVS( 425 )
  W( 80 ) = W( 80 ) + a*JVS( 426 )
  W( 82 ) = W( 82 ) + a*JVS( 427 )
  W( 83 ) = W( 83 ) + a*JVS( 428 )
  W( 84 ) = W( 84 ) + a*JVS( 429 )
  a = -W( 68 ) / JVS(          451  )
  W( 68 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 452 )
  W( 70 ) = W( 70 ) + a*JVS( 453 )
  W( 72 ) = W( 72 ) + a*JVS( 454 )
  W( 73 ) = W( 73 ) + a*JVS( 455 )
  W( 74 ) = W( 74 ) + a*JVS( 456 )
  W( 75 ) = W( 75 ) + a*JVS( 457 )
  W( 76 ) = W( 76 ) + a*JVS( 458 )
  W( 77 ) = W( 77 ) + a*JVS( 459 )
  W( 78 ) = W( 78 ) + a*JVS( 460 )
  W( 79 ) = W( 79 ) + a*JVS( 461 )
  W( 80 ) = W( 80 ) + a*JVS( 462 )
  W( 81 ) = W( 81 ) + a*JVS( 463 )
  W( 82 ) = W( 82 ) + a*JVS( 464 )
  W( 83 ) = W( 83 ) + a*JVS( 465 )
  W( 84 ) = W( 84 ) + a*JVS( 466 )
  W( 85 ) = W( 85 ) + a*JVS( 467 )
  W( 86 ) = W( 86 ) + a*JVS( 468 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  a = -W( 79 ) / JVS(          755  )
  W( 79 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 756 )
  W( 81 ) = W( 81 ) + a*JVS( 757 )
  W( 82 ) = W( 82 ) + a*JVS( 758 )
  W( 83 ) = W( 83 ) + a*JVS( 759 )
  W( 84 ) = W( 84 ) + a*JVS( 760 )
  W( 85 ) = W( 85 ) + a*JVS( 761 )
  W( 86 ) = W( 86 ) + a*JVS( 762 )
  a = -W( 80 ) / JVS(          797  )
  W( 80 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 798 )
  W( 82 ) = W( 82 ) + a*JVS( 799 )
  W( 83 ) = W( 83 ) + a*JVS( 800 )
  W( 84 ) = W( 84 ) + a*JVS( 801 )
  W( 85 ) = W( 85 ) + a*JVS( 802 )
  W( 86 ) = W( 86 ) + a*JVS( 803 )
  a = -W( 81 ) / JVS(          830  )
  W( 81 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 831 )
  W( 83 ) = W( 83 ) + a*JVS( 832 )
  W( 84 ) = W( 84 ) + a*JVS( 833 )
  W( 85 ) = W( 85 ) + a*JVS( 834 )
  W( 86 ) = W( 86 ) + a*JVS( 835 )
  JVS( 836) = W( 14 )
  JVS( 837) = W( 15 )
  JVS( 838) = W( 16 )
  JVS( 839) = W( 19 )
  JVS( 840) = W( 24 )
  JVS( 841) = W( 25 )
  JVS( 842) = W( 26 )
  JVS( 843) = W( 29 )
  JVS( 844) = W( 30 )
  JVS( 845) = W( 32 )
  JVS( 846) = W( 33 )
  JVS( 847) = W( 34 )
  JVS( 848) = W( 35 )
  JVS( 849) = W( 38 )
  JVS( 850) = W( 39 )
  JVS( 851) = W( 40 )
  JVS( 852) = W( 41 )
  JVS( 853) = W( 42 )
  JVS( 854) = W( 43 )
  JVS( 855) = W( 44 )
  JVS( 856) = W( 45 )
  JVS( 857) = W( 47 )
  JVS( 858) = W( 48 )
  JVS( 859) = W( 49 )
  JVS( 860) = W( 50 )
  JVS( 861) = W( 51 )
  JVS( 862) = W( 52 )
  JVS( 863) = W( 53 )
  JVS( 864) = W( 54 )
  JVS( 865) = W( 55 )
  JVS( 866) = W( 56 )
  JVS( 867) = W( 57 )
  JVS( 868) = W( 58 )
  JVS( 869) = W( 59 )
  JVS( 870) = W( 61 )
  JVS( 871) = W( 62 )
  JVS( 872) = W( 63 )
  JVS( 873) = W( 64 )
  JVS( 874) = W( 65 )
  JVS( 875) = W( 66 )
  JVS( 876) = W( 67 )
  JVS( 877) = W( 68 )
  JVS( 878) = W( 69 )
  JVS( 879) = W( 70 )
  JVS( 880) = W( 71 )
  JVS( 881) = W( 72 )
  JVS( 882) = W( 73 )
  JVS( 883) = W( 74 )
  JVS( 884) = W( 75 )
  JVS( 885) = W( 76 )
  JVS( 886) = W( 77 )
  JVS( 887) = W( 78 )
  JVS( 888) = W( 79 )
  JVS( 889) = W( 80 )
  JVS( 890) = W( 81 )
  JVS( 891) = W( 82 )
  JVS( 892) = W( 83 )
  JVS( 893) = W( 84 )
  JVS( 894) = W( 85 )
  JVS( 895) = W( 86 )
  IF ( ABS(  JVS( 918 )) < TINY(a) ) THEN
         IER = 83                                      
         RETURN
  END IF
   W( 21 ) = JVS( 896 )
   W( 56 ) = JVS( 897 )
   W( 57 ) = JVS( 898 )
   W( 59 ) = JVS( 899 )
   W( 61 ) = JVS( 900 )
   W( 62 ) = JVS( 901 )
   W( 64 ) = JVS( 902 )
   W( 65 ) = JVS( 903 )
   W( 66 ) = JVS( 904 )
   W( 70 ) = JVS( 905 )
   W( 71 ) = JVS( 906 )
   W( 72 ) = JVS( 907 )
   W( 73 ) = JVS( 908 )
   W( 74 ) = JVS( 909 )
   W( 75 ) = JVS( 910 )
   W( 76 ) = JVS( 911 )
   W( 77 ) = JVS( 912 )
   W( 78 ) = JVS( 913 )
   W( 79 ) = JVS( 914 )
   W( 80 ) = JVS( 915 )
   W( 81 ) = JVS( 916 )
   W( 82 ) = JVS( 917 )
   W( 83 ) = JVS( 918 )
   W( 84 ) = JVS( 919 )
   W( 85 ) = JVS( 920 )
   W( 86 ) = JVS( 921 )
  a = -W( 21 ) / JVS(          108  )
  W( 21 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 109 )
  W( 83 ) = W( 83 ) + a*JVS( 110 )
  a = -W( 56 ) / JVS(          308  )
  W( 56 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 309 )
  W( 59 ) = W( 59 ) + a*JVS( 310 )
  W( 61 ) = W( 61 ) + a*JVS( 311 )
  W( 64 ) = W( 64 ) + a*JVS( 312 )
  W( 70 ) = W( 70 ) + a*JVS( 313 )
  W( 74 ) = W( 74 ) + a*JVS( 314 )
  W( 80 ) = W( 80 ) + a*JVS( 315 )
  W( 82 ) = W( 82 ) + a*JVS( 316 )
  W( 85 ) = W( 85 ) + a*JVS( 317 )
  a = -W( 57 ) / JVS(          320  )
  W( 57 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 321 )
  W( 76 ) = W( 76 ) + a*JVS( 322 )
  W( 78 ) = W( 78 ) + a*JVS( 323 )
  W( 79 ) = W( 79 ) + a*JVS( 324 )
  W( 80 ) = W( 80 ) + a*JVS( 325 )
  W( 82 ) = W( 82 ) + a*JVS( 326 )
  W( 83 ) = W( 83 ) + a*JVS( 327 )
  W( 84 ) = W( 84 ) + a*JVS( 328 )
  W( 85 ) = W( 85 ) + a*JVS( 329 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  a = -W( 79 ) / JVS(          755  )
  W( 79 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 756 )
  W( 81 ) = W( 81 ) + a*JVS( 757 )
  W( 82 ) = W( 82 ) + a*JVS( 758 )
  W( 83 ) = W( 83 ) + a*JVS( 759 )
  W( 84 ) = W( 84 ) + a*JVS( 760 )
  W( 85 ) = W( 85 ) + a*JVS( 761 )
  W( 86 ) = W( 86 ) + a*JVS( 762 )
  a = -W( 80 ) / JVS(          797  )
  W( 80 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 798 )
  W( 82 ) = W( 82 ) + a*JVS( 799 )
  W( 83 ) = W( 83 ) + a*JVS( 800 )
  W( 84 ) = W( 84 ) + a*JVS( 801 )
  W( 85 ) = W( 85 ) + a*JVS( 802 )
  W( 86 ) = W( 86 ) + a*JVS( 803 )
  a = -W( 81 ) / JVS(          830  )
  W( 81 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 831 )
  W( 83 ) = W( 83 ) + a*JVS( 832 )
  W( 84 ) = W( 84 ) + a*JVS( 833 )
  W( 85 ) = W( 85 ) + a*JVS( 834 )
  W( 86 ) = W( 86 ) + a*JVS( 835 )
  a = -W( 82 ) / JVS(          891  )
  W( 82 ) = -a
  W( 83 ) = W( 83 ) + a*JVS( 892 )
  W( 84 ) = W( 84 ) + a*JVS( 893 )
  W( 85 ) = W( 85 ) + a*JVS( 894 )
  W( 86 ) = W( 86 ) + a*JVS( 895 )
  JVS( 896) = W( 21 )
  JVS( 897) = W( 56 )
  JVS( 898) = W( 57 )
  JVS( 899) = W( 59 )
  JVS( 900) = W( 61 )
  JVS( 901) = W( 62 )
  JVS( 902) = W( 64 )
  JVS( 903) = W( 65 )
  JVS( 904) = W( 66 )
  JVS( 905) = W( 70 )
  JVS( 906) = W( 71 )
  JVS( 907) = W( 72 )
  JVS( 908) = W( 73 )
  JVS( 909) = W( 74 )
  JVS( 910) = W( 75 )
  JVS( 911) = W( 76 )
  JVS( 912) = W( 77 )
  JVS( 913) = W( 78 )
  JVS( 914) = W( 79 )
  JVS( 915) = W( 80 )
  JVS( 916) = W( 81 )
  JVS( 917) = W( 82 )
  JVS( 918) = W( 83 )
  JVS( 919) = W( 84 )
  JVS( 920) = W( 85 )
  JVS( 921) = W( 86 )
  IF ( ABS(  JVS( 936 )) < TINY(a) ) THEN
         IER = 84                                      
         RETURN
  END IF
   W( 22 ) = JVS( 922 )
   W( 47 ) = JVS( 923 )
   W( 65 ) = JVS( 924 )
   W( 70 ) = JVS( 925 )
   W( 74 ) = JVS( 926 )
   W( 75 ) = JVS( 927 )
   W( 76 ) = JVS( 928 )
   W( 77 ) = JVS( 929 )
   W( 78 ) = JVS( 930 )
   W( 79 ) = JVS( 931 )
   W( 80 ) = JVS( 932 )
   W( 81 ) = JVS( 933 )
   W( 82 ) = JVS( 934 )
   W( 83 ) = JVS( 935 )
   W( 84 ) = JVS( 936 )
   W( 85 ) = JVS( 937 )
   W( 86 ) = JVS( 938 )
  a = -W( 22 ) / JVS(          111  )
  W( 22 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 112 )
  W( 84 ) = W( 84 ) + a*JVS( 113 )
  a = -W( 47 ) / JVS(          201  )
  W( 47 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 202 )
  W( 74 ) = W( 74 ) + a*JVS( 203 )
  W( 80 ) = W( 80 ) + a*JVS( 204 )
  W( 82 ) = W( 82 ) + a*JVS( 205 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  a = -W( 79 ) / JVS(          755  )
  W( 79 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 756 )
  W( 81 ) = W( 81 ) + a*JVS( 757 )
  W( 82 ) = W( 82 ) + a*JVS( 758 )
  W( 83 ) = W( 83 ) + a*JVS( 759 )
  W( 84 ) = W( 84 ) + a*JVS( 760 )
  W( 85 ) = W( 85 ) + a*JVS( 761 )
  W( 86 ) = W( 86 ) + a*JVS( 762 )
  a = -W( 80 ) / JVS(          797  )
  W( 80 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 798 )
  W( 82 ) = W( 82 ) + a*JVS( 799 )
  W( 83 ) = W( 83 ) + a*JVS( 800 )
  W( 84 ) = W( 84 ) + a*JVS( 801 )
  W( 85 ) = W( 85 ) + a*JVS( 802 )
  W( 86 ) = W( 86 ) + a*JVS( 803 )
  a = -W( 81 ) / JVS(          830  )
  W( 81 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 831 )
  W( 83 ) = W( 83 ) + a*JVS( 832 )
  W( 84 ) = W( 84 ) + a*JVS( 833 )
  W( 85 ) = W( 85 ) + a*JVS( 834 )
  W( 86 ) = W( 86 ) + a*JVS( 835 )
  a = -W( 82 ) / JVS(          891  )
  W( 82 ) = -a
  W( 83 ) = W( 83 ) + a*JVS( 892 )
  W( 84 ) = W( 84 ) + a*JVS( 893 )
  W( 85 ) = W( 85 ) + a*JVS( 894 )
  W( 86 ) = W( 86 ) + a*JVS( 895 )
  a = -W( 83 ) / JVS(          918  )
  W( 83 ) = -a
  W( 84 ) = W( 84 ) + a*JVS( 919 )
  W( 85 ) = W( 85 ) + a*JVS( 920 )
  W( 86 ) = W( 86 ) + a*JVS( 921 )
  JVS( 922) = W( 22 )
  JVS( 923) = W( 47 )
  JVS( 924) = W( 65 )
  JVS( 925) = W( 70 )
  JVS( 926) = W( 74 )
  JVS( 927) = W( 75 )
  JVS( 928) = W( 76 )
  JVS( 929) = W( 77 )
  JVS( 930) = W( 78 )
  JVS( 931) = W( 79 )
  JVS( 932) = W( 80 )
  JVS( 933) = W( 81 )
  JVS( 934) = W( 82 )
  JVS( 935) = W( 83 )
  JVS( 936) = W( 84 )
  JVS( 937) = W( 85 )
  JVS( 938) = W( 86 )
  IF ( ABS(  JVS( 988 )) < TINY(a) ) THEN
         IER = 85                                      
         RETURN
  END IF
   W( 16 ) = JVS( 939 )
   W( 24 ) = JVS( 940 )
   W( 25 ) = JVS( 941 )
   W( 29 ) = JVS( 942 )
   W( 33 ) = JVS( 943 )
   W( 34 ) = JVS( 944 )
   W( 35 ) = JVS( 945 )
   W( 36 ) = JVS( 946 )
   W( 37 ) = JVS( 947 )
   W( 38 ) = JVS( 948 )
   W( 40 ) = JVS( 949 )
   W( 41 ) = JVS( 950 )
   W( 42 ) = JVS( 951 )
   W( 44 ) = JVS( 952 )
   W( 45 ) = JVS( 953 )
   W( 46 ) = JVS( 954 )
   W( 48 ) = JVS( 955 )
   W( 50 ) = JVS( 956 )
   W( 51 ) = JVS( 957 )
   W( 53 ) = JVS( 958 )
   W( 55 ) = JVS( 959 )
   W( 56 ) = JVS( 960 )
   W( 57 ) = JVS( 961 )
   W( 58 ) = JVS( 962 )
   W( 59 ) = JVS( 963 )
   W( 61 ) = JVS( 964 )
   W( 62 ) = JVS( 965 )
   W( 63 ) = JVS( 966 )
   W( 64 ) = JVS( 967 )
   W( 65 ) = JVS( 968 )
   W( 66 ) = JVS( 969 )
   W( 67 ) = JVS( 970 )
   W( 68 ) = JVS( 971 )
   W( 69 ) = JVS( 972 )
   W( 70 ) = JVS( 973 )
   W( 71 ) = JVS( 974 )
   W( 72 ) = JVS( 975 )
   W( 73 ) = JVS( 976 )
   W( 74 ) = JVS( 977 )
   W( 75 ) = JVS( 978 )
   W( 76 ) = JVS( 979 )
   W( 77 ) = JVS( 980 )
   W( 78 ) = JVS( 981 )
   W( 79 ) = JVS( 982 )
   W( 80 ) = JVS( 983 )
   W( 81 ) = JVS( 984 )
   W( 82 ) = JVS( 985 )
   W( 83 ) = JVS( 986 )
   W( 84 ) = JVS( 987 )
   W( 85 ) = JVS( 988 )
   W( 86 ) = JVS( 989 )
  a = -W( 16 ) / JVS(           91  )
  W( 16 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 92 )
  a = -W( 24 ) / JVS(          117  )
  W( 24 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 118 )
  W( 85 ) = W( 85 ) + a*JVS( 119 )
  a = -W( 25 ) / JVS(          120  )
  W( 25 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 121 )
  a = -W( 29 ) / JVS(          133  )
  W( 29 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 134 )
  W( 82 ) = W( 82 ) + a*JVS( 135 )
  a = -W( 33 ) / JVS(          144  )
  W( 33 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 145 )
  W( 81 ) = W( 81 ) + a*JVS( 146 )
  W( 82 ) = W( 82 ) + a*JVS( 147 )
  W( 86 ) = W( 86 ) + a*JVS( 148 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 35 ) / JVS(          151  )
  W( 35 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 152 )
  W( 82 ) = W( 82 ) + a*JVS( 153 )
  W( 85 ) = W( 85 ) + a*JVS( 154 )
  a = -W( 36 ) / JVS(          155  )
  W( 36 ) = -a
  W( 68 ) = W( 68 ) + a*JVS( 156 )
  W( 78 ) = W( 78 ) + a*JVS( 157 )
  W( 85 ) = W( 85 ) + a*JVS( 158 )
  a = -W( 37 ) / JVS(          159  )
  W( 37 ) = -a
  W( 46 ) = W( 46 ) + a*JVS( 160 )
  W( 79 ) = W( 79 ) + a*JVS( 161 )
  W( 80 ) = W( 80 ) + a*JVS( 162 )
  W( 85 ) = W( 85 ) + a*JVS( 163 )
  a = -W( 38 ) / JVS(          164  )
  W( 38 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 165 )
  W( 82 ) = W( 82 ) + a*JVS( 166 )
  W( 85 ) = W( 85 ) + a*JVS( 167 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  a = -W( 41 ) / JVS(          174  )
  W( 41 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 175 )
  a = -W( 42 ) / JVS(          178  )
  W( 42 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 179 )
  a = -W( 44 ) / JVS(          185  )
  W( 44 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 186 )
  W( 82 ) = W( 82 ) + a*JVS( 187 )
  a = -W( 45 ) / JVS(          190  )
  W( 45 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 191 )
  W( 82 ) = W( 82 ) + a*JVS( 192 )
  a = -W( 46 ) / JVS(          194  )
  W( 46 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 195 )
  W( 79 ) = W( 79 ) + a*JVS( 196 )
  W( 80 ) = W( 80 ) + a*JVS( 197 )
  W( 85 ) = W( 85 ) + a*JVS( 198 )
  a = -W( 48 ) / JVS(          206  )
  W( 48 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 207 )
  W( 82 ) = W( 82 ) + a*JVS( 208 )
  W( 85 ) = W( 85 ) + a*JVS( 209 )
  W( 86 ) = W( 86 ) + a*JVS( 210 )
  a = -W( 50 ) / JVS(          222  )
  W( 50 ) = -a
  W( 62 ) = W( 62 ) + a*JVS( 223 )
  W( 64 ) = W( 64 ) + a*JVS( 224 )
  W( 66 ) = W( 66 ) + a*JVS( 225 )
  W( 74 ) = W( 74 ) + a*JVS( 226 )
  W( 80 ) = W( 80 ) + a*JVS( 227 )
  W( 82 ) = W( 82 ) + a*JVS( 228 )
  a = -W( 51 ) / JVS(          235  )
  W( 51 ) = -a
  W( 53 ) = W( 53 ) + a*JVS( 236 )
  W( 55 ) = W( 55 ) + a*JVS( 237 )
  W( 56 ) = W( 56 ) + a*JVS( 238 )
  W( 58 ) = W( 58 ) + a*JVS( 239 )
  W( 59 ) = W( 59 ) + a*JVS( 240 )
  W( 61 ) = W( 61 ) + a*JVS( 241 )
  W( 62 ) = W( 62 ) + a*JVS( 242 )
  W( 63 ) = W( 63 ) + a*JVS( 243 )
  W( 64 ) = W( 64 ) + a*JVS( 244 )
  W( 65 ) = W( 65 ) + a*JVS( 245 )
  W( 66 ) = W( 66 ) + a*JVS( 246 )
  W( 67 ) = W( 67 ) + a*JVS( 247 )
  W( 68 ) = W( 68 ) + a*JVS( 248 )
  W( 70 ) = W( 70 ) + a*JVS( 249 )
  W( 71 ) = W( 71 ) + a*JVS( 250 )
  W( 74 ) = W( 74 ) + a*JVS( 251 )
  W( 80 ) = W( 80 ) + a*JVS( 252 )
  W( 82 ) = W( 82 ) + a*JVS( 253 )
  a = -W( 53 ) / JVS(          275  )
  W( 53 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 276 )
  W( 74 ) = W( 74 ) + a*JVS( 277 )
  W( 80 ) = W( 80 ) + a*JVS( 278 )
  W( 82 ) = W( 82 ) + a*JVS( 279 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 56 ) / JVS(          308  )
  W( 56 ) = -a
  W( 57 ) = W( 57 ) + a*JVS( 309 )
  W( 59 ) = W( 59 ) + a*JVS( 310 )
  W( 61 ) = W( 61 ) + a*JVS( 311 )
  W( 64 ) = W( 64 ) + a*JVS( 312 )
  W( 70 ) = W( 70 ) + a*JVS( 313 )
  W( 74 ) = W( 74 ) + a*JVS( 314 )
  W( 80 ) = W( 80 ) + a*JVS( 315 )
  W( 82 ) = W( 82 ) + a*JVS( 316 )
  W( 85 ) = W( 85 ) + a*JVS( 317 )
  a = -W( 57 ) / JVS(          320  )
  W( 57 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 321 )
  W( 76 ) = W( 76 ) + a*JVS( 322 )
  W( 78 ) = W( 78 ) + a*JVS( 323 )
  W( 79 ) = W( 79 ) + a*JVS( 324 )
  W( 80 ) = W( 80 ) + a*JVS( 325 )
  W( 82 ) = W( 82 ) + a*JVS( 326 )
  W( 83 ) = W( 83 ) + a*JVS( 327 )
  W( 84 ) = W( 84 ) + a*JVS( 328 )
  W( 85 ) = W( 85 ) + a*JVS( 329 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 62 ) / JVS(          376  )
  W( 62 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 377 )
  W( 70 ) = W( 70 ) + a*JVS( 378 )
  W( 74 ) = W( 74 ) + a*JVS( 379 )
  W( 80 ) = W( 80 ) + a*JVS( 380 )
  W( 82 ) = W( 82 ) + a*JVS( 381 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 67 ) / JVS(          416  )
  W( 67 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 417 )
  W( 70 ) = W( 70 ) + a*JVS( 418 )
  W( 71 ) = W( 71 ) + a*JVS( 419 )
  W( 72 ) = W( 72 ) + a*JVS( 420 )
  W( 73 ) = W( 73 ) + a*JVS( 421 )
  W( 74 ) = W( 74 ) + a*JVS( 422 )
  W( 75 ) = W( 75 ) + a*JVS( 423 )
  W( 76 ) = W( 76 ) + a*JVS( 424 )
  W( 78 ) = W( 78 ) + a*JVS( 425 )
  W( 80 ) = W( 80 ) + a*JVS( 426 )
  W( 82 ) = W( 82 ) + a*JVS( 427 )
  W( 83 ) = W( 83 ) + a*JVS( 428 )
  W( 84 ) = W( 84 ) + a*JVS( 429 )
  a = -W( 68 ) / JVS(          451  )
  W( 68 ) = -a
  W( 69 ) = W( 69 ) + a*JVS( 452 )
  W( 70 ) = W( 70 ) + a*JVS( 453 )
  W( 72 ) = W( 72 ) + a*JVS( 454 )
  W( 73 ) = W( 73 ) + a*JVS( 455 )
  W( 74 ) = W( 74 ) + a*JVS( 456 )
  W( 75 ) = W( 75 ) + a*JVS( 457 )
  W( 76 ) = W( 76 ) + a*JVS( 458 )
  W( 77 ) = W( 77 ) + a*JVS( 459 )
  W( 78 ) = W( 78 ) + a*JVS( 460 )
  W( 79 ) = W( 79 ) + a*JVS( 461 )
  W( 80 ) = W( 80 ) + a*JVS( 462 )
  W( 81 ) = W( 81 ) + a*JVS( 463 )
  W( 82 ) = W( 82 ) + a*JVS( 464 )
  W( 83 ) = W( 83 ) + a*JVS( 465 )
  W( 84 ) = W( 84 ) + a*JVS( 466 )
  W( 85 ) = W( 85 ) + a*JVS( 467 )
  W( 86 ) = W( 86 ) + a*JVS( 468 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  a = -W( 79 ) / JVS(          755  )
  W( 79 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 756 )
  W( 81 ) = W( 81 ) + a*JVS( 757 )
  W( 82 ) = W( 82 ) + a*JVS( 758 )
  W( 83 ) = W( 83 ) + a*JVS( 759 )
  W( 84 ) = W( 84 ) + a*JVS( 760 )
  W( 85 ) = W( 85 ) + a*JVS( 761 )
  W( 86 ) = W( 86 ) + a*JVS( 762 )
  a = -W( 80 ) / JVS(          797  )
  W( 80 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 798 )
  W( 82 ) = W( 82 ) + a*JVS( 799 )
  W( 83 ) = W( 83 ) + a*JVS( 800 )
  W( 84 ) = W( 84 ) + a*JVS( 801 )
  W( 85 ) = W( 85 ) + a*JVS( 802 )
  W( 86 ) = W( 86 ) + a*JVS( 803 )
  a = -W( 81 ) / JVS(          830  )
  W( 81 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 831 )
  W( 83 ) = W( 83 ) + a*JVS( 832 )
  W( 84 ) = W( 84 ) + a*JVS( 833 )
  W( 85 ) = W( 85 ) + a*JVS( 834 )
  W( 86 ) = W( 86 ) + a*JVS( 835 )
  a = -W( 82 ) / JVS(          891  )
  W( 82 ) = -a
  W( 83 ) = W( 83 ) + a*JVS( 892 )
  W( 84 ) = W( 84 ) + a*JVS( 893 )
  W( 85 ) = W( 85 ) + a*JVS( 894 )
  W( 86 ) = W( 86 ) + a*JVS( 895 )
  a = -W( 83 ) / JVS(          918  )
  W( 83 ) = -a
  W( 84 ) = W( 84 ) + a*JVS( 919 )
  W( 85 ) = W( 85 ) + a*JVS( 920 )
  W( 86 ) = W( 86 ) + a*JVS( 921 )
  a = -W( 84 ) / JVS(          936  )
  W( 84 ) = -a
  W( 85 ) = W( 85 ) + a*JVS( 937 )
  W( 86 ) = W( 86 ) + a*JVS( 938 )
  JVS( 939) = W( 16 )
  JVS( 940) = W( 24 )
  JVS( 941) = W( 25 )
  JVS( 942) = W( 29 )
  JVS( 943) = W( 33 )
  JVS( 944) = W( 34 )
  JVS( 945) = W( 35 )
  JVS( 946) = W( 36 )
  JVS( 947) = W( 37 )
  JVS( 948) = W( 38 )
  JVS( 949) = W( 40 )
  JVS( 950) = W( 41 )
  JVS( 951) = W( 42 )
  JVS( 952) = W( 44 )
  JVS( 953) = W( 45 )
  JVS( 954) = W( 46 )
  JVS( 955) = W( 48 )
  JVS( 956) = W( 50 )
  JVS( 957) = W( 51 )
  JVS( 958) = W( 53 )
  JVS( 959) = W( 55 )
  JVS( 960) = W( 56 )
  JVS( 961) = W( 57 )
  JVS( 962) = W( 58 )
  JVS( 963) = W( 59 )
  JVS( 964) = W( 61 )
  JVS( 965) = W( 62 )
  JVS( 966) = W( 63 )
  JVS( 967) = W( 64 )
  JVS( 968) = W( 65 )
  JVS( 969) = W( 66 )
  JVS( 970) = W( 67 )
  JVS( 971) = W( 68 )
  JVS( 972) = W( 69 )
  JVS( 973) = W( 70 )
  JVS( 974) = W( 71 )
  JVS( 975) = W( 72 )
  JVS( 976) = W( 73 )
  JVS( 977) = W( 74 )
  JVS( 978) = W( 75 )
  JVS( 979) = W( 76 )
  JVS( 980) = W( 77 )
  JVS( 981) = W( 78 )
  JVS( 982) = W( 79 )
  JVS( 983) = W( 80 )
  JVS( 984) = W( 81 )
  JVS( 985) = W( 82 )
  JVS( 986) = W( 83 )
  JVS( 987) = W( 84 )
  JVS( 988) = W( 85 )
  JVS( 989) = W( 86 )
  IF ( ABS(  JVS( 1021 )) < TINY(a) ) THEN
         IER = 86                                      
         RETURN
  END IF
   W( 26 ) = JVS( 990 )
   W( 30 ) = JVS( 991 )
   W( 32 ) = JVS( 992 )
   W( 34 ) = JVS( 993 )
   W( 39 ) = JVS( 994 )
   W( 40 ) = JVS( 995 )
   W( 55 ) = JVS( 996 )
   W( 58 ) = JVS( 997 )
   W( 59 ) = JVS( 998 )
   W( 61 ) = JVS( 999 )
   W( 63 ) = JVS( 1000 )
   W( 64 ) = JVS( 1001 )
   W( 65 ) = JVS( 1002 )
   W( 66 ) = JVS( 1003 )
   W( 69 ) = JVS( 1004 )
   W( 70 ) = JVS( 1005 )
   W( 71 ) = JVS( 1006 )
   W( 72 ) = JVS( 1007 )
   W( 73 ) = JVS( 1008 )
   W( 74 ) = JVS( 1009 )
   W( 75 ) = JVS( 1010 )
   W( 76 ) = JVS( 1011 )
   W( 77 ) = JVS( 1012 )
   W( 78 ) = JVS( 1013 )
   W( 79 ) = JVS( 1014 )
   W( 80 ) = JVS( 1015 )
   W( 81 ) = JVS( 1016 )
   W( 82 ) = JVS( 1017 )
   W( 83 ) = JVS( 1018 )
   W( 84 ) = JVS( 1019 )
   W( 85 ) = JVS( 1020 )
   W( 86 ) = JVS( 1021 )
  a = -W( 26 ) / JVS(          122  )
  W( 26 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 123 )
  a = -W( 30 ) / JVS(          136  )
  W( 30 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 137 )
  a = -W( 32 ) / JVS(          142  )
  W( 32 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 143 )
  a = -W( 34 ) / JVS(          149  )
  W( 34 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 150 )
  a = -W( 39 ) / JVS(          168  )
  W( 39 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 169 )
  a = -W( 40 ) / JVS(          170  )
  W( 40 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 171 )
  a = -W( 55 ) / JVS(          295  )
  W( 55 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 296 )
  W( 74 ) = W( 74 ) + a*JVS( 297 )
  W( 80 ) = W( 80 ) + a*JVS( 298 )
  W( 82 ) = W( 82 ) + a*JVS( 299 )
  a = -W( 58 ) / JVS(          330  )
  W( 58 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 331 )
  W( 74 ) = W( 74 ) + a*JVS( 332 )
  W( 80 ) = W( 80 ) + a*JVS( 333 )
  W( 82 ) = W( 82 ) + a*JVS( 334 )
  a = -W( 59 ) / JVS(          335  )
  W( 59 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 336 )
  W( 74 ) = W( 74 ) + a*JVS( 337 )
  W( 80 ) = W( 80 ) + a*JVS( 338 )
  W( 82 ) = W( 82 ) + a*JVS( 339 )
  a = -W( 61 ) / JVS(          370  )
  W( 61 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 371 )
  W( 74 ) = W( 74 ) + a*JVS( 372 )
  W( 80 ) = W( 80 ) + a*JVS( 373 )
  W( 82 ) = W( 82 ) + a*JVS( 374 )
  a = -W( 63 ) / JVS(          382  )
  W( 63 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 383 )
  W( 74 ) = W( 74 ) + a*JVS( 384 )
  W( 80 ) = W( 80 ) + a*JVS( 385 )
  W( 82 ) = W( 82 ) + a*JVS( 386 )
  a = -W( 64 ) / JVS(          388  )
  W( 64 ) = -a
  W( 65 ) = W( 65 ) + a*JVS( 389 )
  W( 70 ) = W( 70 ) + a*JVS( 390 )
  W( 74 ) = W( 74 ) + a*JVS( 391 )
  W( 80 ) = W( 80 ) + a*JVS( 392 )
  W( 82 ) = W( 82 ) + a*JVS( 393 )
  a = -W( 65 ) / JVS(          394  )
  W( 65 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 395 )
  W( 74 ) = W( 74 ) + a*JVS( 396 )
  W( 80 ) = W( 80 ) + a*JVS( 397 )
  W( 82 ) = W( 82 ) + a*JVS( 398 )
  a = -W( 66 ) / JVS(          401  )
  W( 66 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 402 )
  W( 74 ) = W( 74 ) + a*JVS( 403 )
  W( 80 ) = W( 80 ) + a*JVS( 404 )
  W( 82 ) = W( 82 ) + a*JVS( 405 )
  a = -W( 69 ) / JVS(          475  )
  W( 69 ) = -a
  W( 70 ) = W( 70 ) + a*JVS( 476 )
  W( 74 ) = W( 74 ) + a*JVS( 477 )
  W( 78 ) = W( 78 ) + a*JVS( 478 )
  W( 79 ) = W( 79 ) + a*JVS( 479 )
  W( 80 ) = W( 80 ) + a*JVS( 480 )
  W( 82 ) = W( 82 ) + a*JVS( 481 )
  W( 86 ) = W( 86 ) + a*JVS( 482 )
  a = -W( 70 ) / JVS(          493  )
  W( 70 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 494 )
  W( 78 ) = W( 78 ) + a*JVS( 495 )
  W( 79 ) = W( 79 ) + a*JVS( 496 )
  W( 80 ) = W( 80 ) + a*JVS( 497 )
  W( 82 ) = W( 82 ) + a*JVS( 498 )
  a = -W( 71 ) / JVS(          518  )
  W( 71 ) = -a
  W( 72 ) = W( 72 ) + a*JVS( 519 )
  W( 73 ) = W( 73 ) + a*JVS( 520 )
  W( 74 ) = W( 74 ) + a*JVS( 521 )
  W( 77 ) = W( 77 ) + a*JVS( 522 )
  W( 78 ) = W( 78 ) + a*JVS( 523 )
  W( 79 ) = W( 79 ) + a*JVS( 524 )
  W( 80 ) = W( 80 ) + a*JVS( 525 )
  W( 82 ) = W( 82 ) + a*JVS( 526 )
  W( 85 ) = W( 85 ) + a*JVS( 527 )
  W( 86 ) = W( 86 ) + a*JVS( 528 )
  a = -W( 72 ) / JVS(          540  )
  W( 72 ) = -a
  W( 73 ) = W( 73 ) + a*JVS( 541 )
  W( 74 ) = W( 74 ) + a*JVS( 542 )
  W( 77 ) = W( 77 ) + a*JVS( 543 )
  W( 78 ) = W( 78 ) + a*JVS( 544 )
  W( 79 ) = W( 79 ) + a*JVS( 545 )
  W( 80 ) = W( 80 ) + a*JVS( 546 )
  W( 81 ) = W( 81 ) + a*JVS( 547 )
  W( 82 ) = W( 82 ) + a*JVS( 548 )
  W( 86 ) = W( 86 ) + a*JVS( 549 )
  a = -W( 73 ) / JVS(          562  )
  W( 73 ) = -a
  W( 74 ) = W( 74 ) + a*JVS( 563 )
  W( 76 ) = W( 76 ) + a*JVS( 564 )
  W( 77 ) = W( 77 ) + a*JVS( 565 )
  W( 78 ) = W( 78 ) + a*JVS( 566 )
  W( 79 ) = W( 79 ) + a*JVS( 567 )
  W( 80 ) = W( 80 ) + a*JVS( 568 )
  W( 81 ) = W( 81 ) + a*JVS( 569 )
  W( 82 ) = W( 82 ) + a*JVS( 570 )
  W( 83 ) = W( 83 ) + a*JVS( 571 )
  W( 84 ) = W( 84 ) + a*JVS( 572 )
  W( 86 ) = W( 86 ) + a*JVS( 573 )
  a = -W( 74 ) / JVS(          587  )
  W( 74 ) = -a
  W( 75 ) = W( 75 ) + a*JVS( 588 )
  W( 76 ) = W( 76 ) + a*JVS( 589 )
  W( 78 ) = W( 78 ) + a*JVS( 590 )
  W( 79 ) = W( 79 ) + a*JVS( 591 )
  W( 80 ) = W( 80 ) + a*JVS( 592 )
  W( 82 ) = W( 82 ) + a*JVS( 593 )
  W( 83 ) = W( 83 ) + a*JVS( 594 )
  W( 84 ) = W( 84 ) + a*JVS( 595 )
  W( 85 ) = W( 85 ) + a*JVS( 596 )
  a = -W( 75 ) / JVS(          605  )
  W( 75 ) = -a
  W( 76 ) = W( 76 ) + a*JVS( 606 )
  W( 77 ) = W( 77 ) + a*JVS( 607 )
  W( 78 ) = W( 78 ) + a*JVS( 608 )
  W( 79 ) = W( 79 ) + a*JVS( 609 )
  W( 80 ) = W( 80 ) + a*JVS( 610 )
  W( 81 ) = W( 81 ) + a*JVS( 611 )
  W( 82 ) = W( 82 ) + a*JVS( 612 )
  W( 83 ) = W( 83 ) + a*JVS( 613 )
  W( 84 ) = W( 84 ) + a*JVS( 614 )
  W( 85 ) = W( 85 ) + a*JVS( 615 )
  W( 86 ) = W( 86 ) + a*JVS( 616 )
  a = -W( 76 ) / JVS(          640  )
  W( 76 ) = -a
  W( 77 ) = W( 77 ) + a*JVS( 641 )
  W( 78 ) = W( 78 ) + a*JVS( 642 )
  W( 79 ) = W( 79 ) + a*JVS( 643 )
  W( 80 ) = W( 80 ) + a*JVS( 644 )
  W( 81 ) = W( 81 ) + a*JVS( 645 )
  W( 82 ) = W( 82 ) + a*JVS( 646 )
  W( 83 ) = W( 83 ) + a*JVS( 647 )
  W( 84 ) = W( 84 ) + a*JVS( 648 )
  W( 85 ) = W( 85 ) + a*JVS( 649 )
  W( 86 ) = W( 86 ) + a*JVS( 650 )
  a = -W( 77 ) / JVS(          685  )
  W( 77 ) = -a
  W( 78 ) = W( 78 ) + a*JVS( 686 )
  W( 79 ) = W( 79 ) + a*JVS( 687 )
  W( 80 ) = W( 80 ) + a*JVS( 688 )
  W( 81 ) = W( 81 ) + a*JVS( 689 )
  W( 82 ) = W( 82 ) + a*JVS( 690 )
  W( 83 ) = W( 83 ) + a*JVS( 691 )
  W( 84 ) = W( 84 ) + a*JVS( 692 )
  W( 85 ) = W( 85 ) + a*JVS( 693 )
  W( 86 ) = W( 86 ) + a*JVS( 694 )
  a = -W( 78 ) / JVS(          711  )
  W( 78 ) = -a
  W( 79 ) = W( 79 ) + a*JVS( 712 )
  W( 80 ) = W( 80 ) + a*JVS( 713 )
  W( 81 ) = W( 81 ) + a*JVS( 714 )
  W( 82 ) = W( 82 ) + a*JVS( 715 )
  W( 83 ) = W( 83 ) + a*JVS( 716 )
  W( 84 ) = W( 84 ) + a*JVS( 717 )
  W( 85 ) = W( 85 ) + a*JVS( 718 )
  W( 86 ) = W( 86 ) + a*JVS( 719 )
  a = -W( 79 ) / JVS(          755  )
  W( 79 ) = -a
  W( 80 ) = W( 80 ) + a*JVS( 756 )
  W( 81 ) = W( 81 ) + a*JVS( 757 )
  W( 82 ) = W( 82 ) + a*JVS( 758 )
  W( 83 ) = W( 83 ) + a*JVS( 759 )
  W( 84 ) = W( 84 ) + a*JVS( 760 )
  W( 85 ) = W( 85 ) + a*JVS( 761 )
  W( 86 ) = W( 86 ) + a*JVS( 762 )
  a = -W( 80 ) / JVS(          797  )
  W( 80 ) = -a
  W( 81 ) = W( 81 ) + a*JVS( 798 )
  W( 82 ) = W( 82 ) + a*JVS( 799 )
  W( 83 ) = W( 83 ) + a*JVS( 800 )
  W( 84 ) = W( 84 ) + a*JVS( 801 )
  W( 85 ) = W( 85 ) + a*JVS( 802 )
  W( 86 ) = W( 86 ) + a*JVS( 803 )
  a = -W( 81 ) / JVS(          830  )
  W( 81 ) = -a
  W( 82 ) = W( 82 ) + a*JVS( 831 )
  W( 83 ) = W( 83 ) + a*JVS( 832 )
  W( 84 ) = W( 84 ) + a*JVS( 833 )
  W( 85 ) = W( 85 ) + a*JVS( 834 )
  W( 86 ) = W( 86 ) + a*JVS( 835 )
  a = -W( 82 ) / JVS(          891  )
  W( 82 ) = -a
  W( 83 ) = W( 83 ) + a*JVS( 892 )
  W( 84 ) = W( 84 ) + a*JVS( 893 )
  W( 85 ) = W( 85 ) + a*JVS( 894 )
  W( 86 ) = W( 86 ) + a*JVS( 895 )
  a = -W( 83 ) / JVS(          918  )
  W( 83 ) = -a
  W( 84 ) = W( 84 ) + a*JVS( 919 )
  W( 85 ) = W( 85 ) + a*JVS( 920 )
  W( 86 ) = W( 86 ) + a*JVS( 921 )
  a = -W( 84 ) / JVS(          936  )
  W( 84 ) = -a
  W( 85 ) = W( 85 ) + a*JVS( 937 )
  W( 86 ) = W( 86 ) + a*JVS( 938 )
  a = -W( 85 ) / JVS(          988  )
  W( 85 ) = -a
  W( 86 ) = W( 86 ) + a*JVS( 989 )
  JVS( 990) = W( 26 )
  JVS( 991) = W( 30 )
  JVS( 992) = W( 32 )
  JVS( 993) = W( 34 )
  JVS( 994) = W( 39 )
  JVS( 995) = W( 40 )
  JVS( 996) = W( 55 )
  JVS( 997) = W( 58 )
  JVS( 998) = W( 59 )
  JVS( 999) = W( 61 )
  JVS( 1000) = W( 63 )
  JVS( 1001) = W( 64 )
  JVS( 1002) = W( 65 )
  JVS( 1003) = W( 66 )
  JVS( 1004) = W( 69 )
  JVS( 1005) = W( 70 )
  JVS( 1006) = W( 71 )
  JVS( 1007) = W( 72 )
  JVS( 1008) = W( 73 )
  JVS( 1009) = W( 74 )
  JVS( 1010) = W( 75 )
  JVS( 1011) = W( 76 )
  JVS( 1012) = W( 77 )
  JVS( 1013) = W( 78 )
  JVS( 1014) = W( 79 )
  JVS( 1015) = W( 80 )
  JVS( 1016) = W( 81 )
  JVS( 1017) = W( 82 )
  JVS( 1018) = W( 83 )
  JVS( 1019) = W( 84 )
  JVS( 1020) = W( 85 )
  JVS( 1021) = W( 86 )
   
   END SUBROUTINE decomp_saprc99
 


END MODULE saprc99_Integrator
