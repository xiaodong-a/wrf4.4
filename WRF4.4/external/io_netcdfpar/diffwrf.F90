module read_util_module

contains

   subroutine arguments(v2file, lmore)
     implicit none
     character(len=*) :: v2file
     character(len=120) :: harg
     logical :: lmore
   
     integer :: ierr, i, numarg
   
     numarg = command_argument_count()
   
     i = 1
     lmore = .false.
   
     do while ( i < numarg) 
        call get_command_argument(number=i, value=harg)
        print*, 'harg = ', trim(harg)
   
        if (harg == "-v") then
           i = i + 1
           lmore = .true.
        elseif (harg == "-h") then
           call help
        endif
   
     enddo
   
     call get_command_argument(number=i, value=harg)
     v2file = harg
   end subroutine arguments
   
   subroutine help
     implicit none
     character(len=120) :: cmd
     call get_command_argument(number=0, value=cmd)
   
     write(*,'(/,"Usage: ", A, " [-v] v2file ")') trim(cmd)
     write(*,'(8x, "-v     : Print extra info")')
     write(*,'(8x, "v3file : MM5v3 file name to read.")')
     write(*,'(8x, "-h     : print this help message and exit.",/)')
     stop
   end subroutine help
end module read_util_module



 program readv3
  use wrf_data_ncpar
  use read_util_module
  implicit none
#include "wrf_status_codes.h"
#include "netcdf.inc"
  character(len=255) :: flnm
  character(len=255) :: flnm2
  character(len=120) :: arg3
  character(len=19) :: DateStr
  character(len=19) :: DateStr2
  character(len=31) :: VarName
  character(len=31) :: VarName2
  integer dh1, dh2

  integer :: flag, flag2
  integer :: iunit, iunit2

  integer :: i,j,k
  integer :: levlim
  integer :: cross
  integer :: ndim, ndim2
  integer :: WrfType, WrfType2
  real :: time, time2
  real*8 :: a, b
  real*8 :: sumE, sum1, sum2, diff1, diff2, serr, perr, rmse, rms1, rms2, tmp1, tmp2
  integer digits,d1, d2
  integer, dimension(4) :: start_index, end_index, start_index2, end_index2
  integer , Dimension(3) :: MemS,MemE,PatS,PatE
  character (len= 4) :: staggering,   staggering2
  character (len= 3) :: ordering,     ordering2, ord
  character (len=24) :: start_date,   start_date2
  character (len=24) :: current_date, current_date2
  character (len=31) :: name,         name2,  tmpname
  character (len=25) :: units,        units2
  character (len=46) :: description,  description2

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo

  logical :: first, searchcoords
  integer :: l, n, ntimes
  integer :: ikdiffs, ifdiffs
  integer :: icenter, prev_icenter, jcenter, prev_jcenter,ntries
  real :: searchlat, searchlong

  real, allocatable, dimension(:,:,:,:) :: data,data2
  real, allocatable, dimension(:,:)     :: xlat,xlong

  integer :: ierr, ierr2, ier, ier2, Status, Status_next_time, Status_next_time2, Status_next_var, Status_next_var_2
  integer :: nargs

  logical :: newtime = .TRUE.
  logical :: justplot, efound

  logical, external :: iveceq

  levlim = -1

  call ext_ncdpar_ioinit(SysDepInfo,Status)
  call set_wrf_debug_level ( 1 )

  nargs = command_argument_count()

  Justplot = .false.
  searchcoords = .false.
! get arguments
  if ( nargs .ge. 2 ) then
    call get_command_argument(number=1, value=flnm)
    call get_command_argument(number=2, value=flnm2)
    IF ( flnm2(1:4) .EQ. '-lat' ) THEN
print*,'reading ',TRIM(flnm2(5:))
      read(flnm2(5:),*)searchlat
      call get_command_argument(number=3, value=flnm2)
      IF ( flnm2(1:5) .EQ. '-long' ) THEN
print*,'reading ',TRIM(flnm2(6:))
        read(flnm2(6:),*)searchlong
      ELSE
        write(*,*)'missing -long argument (no spaces after -lat or -long, either)'
        STOP
      ENDIF
      nargs = 0
      Justplot = .true.
      searchcoords = .true.
      call ext_ncdpar_open_for_read( trim(flnm), 0, 0, "", dh1, Status)
      goto 924
    ENDIF
    ierr = 0
    call ext_ncdpar_open_for_read( trim(flnm), 0, 0, "", dh1, Status)
    if ( Status /= 0 ) then 
      print*,'error opening ',flnm, ' Status = ', Status ; stop 
    endif
    call ext_ncdpar_open_for_read( trim(flnm2), 0, 0, "", dh2, Status)
    if ( Status /= 0 ) go to 923
    goto 924
923    continue

! bounce here if second name is not openable -- this would mean that
! it is a field name instead.

    print*,'could not open ',flnm2
    name = flnm2
    Justplot = .true.
924    continue
  if ( nargs .eq. 3 ) then
    call get_command_argument(number=3, value=arg3)
    read(arg3,*)levlim
    print*,'LEVLIM = ',LEVLIM
  endif
  else
     print*,'Usage: command file1 file2'
     stop
  endif

print*,'Just plot ',Justplot

if ( Justplot ) then
  print*, 'flnm = ', trim(flnm)
  first = .TRUE.

  call ext_ncdpar_get_next_time(dh1, DateStr, Status_next_time)

  ntimes = 0
  DO WHILE ( Status_next_time .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)
    ntimes = ntimes + 1
    call ext_ncdpar_get_next_var (dh1, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )
!    write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      call ext_ncdpar_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )
      if(WrfType /= WRF_REAL .AND. WrfType /= WRF_DOUBLE) then 
        call ext_ncdpar_get_next_var (dh1, VarName, Status_next_var) 
        cycle 
      endif 
      IF ( .NOT. searchcoords ) THEN
        write(*,'(A9,1x,I1,3(1x,I5),1x,A,1x,A)')&
                 VarName, ndim, end_index(1), end_index(2), end_index(3), &
                 trim(ordering), trim(DateStr)
      ENDIF

      if ( VarName .eq. name .OR. TRIM(VarName) .EQ. 'XLAT' .OR. TRIM(VarName) .EQ. 'XLONG' ) then
        write(*,*)'Writing fort.88 file for ', trim(name)

        allocate(data(end_index(1), end_index(2), end_index(3), 1))

        if ( ndim .eq. 3 ) then
          ord = 'XYZ'
        else if ( ndim .eq. 2 ) then
          ord = 'XY'
        else if ( ndim .eq. 1 ) then
          ord = 'Z'
        else if ( ndim .eq. 0 ) then
          ord = '0'
        endif

        call ext_ncdpar_read_field(dh1,DateStr,TRIM(VarName),data,WRF_REAL,0,0,0,ord, &
                            staggering, dimnames ,                      &
                            start_index,end_index,                      & !dom
                            start_index,end_index,                      & !mem
                            start_index,end_index,                      & !pat
                            ierr)

        if ( ierr/=0 ) then
             write(*,*)'error reading data record'
             write(*,*)'  ndim = ', ndim
             write(*,*)'  end_index(1) ',end_index(1)
             write(*,*)'  end_index(2) ',end_index(2)
             write(*,*)'  end_index(3) ',end_index(3)
        endif

write(*,*)'name: ',TRIM(VarName)
        IF ( TRIM(VarName) .EQ. 'XLAT' .AND. .NOT. ALLOCATED(xlat)) THEN
write(*,*)'allocating xlat'
           ALLOCATE(xlat(end_index(1), end_index(2)))
           xlat = data(:,:,1,1)
        ENDIF
        IF ( TRIM(VarName) .EQ. 'XLONG' .AND. .NOT. ALLOCATED(xlong)) THEN
write(*,*)'allocating xlong'
           ALLOCATE(xlong(end_index(1), end_index(2)))
           xlong = data(:,:,1,1)
        ENDIF


        if ( VarName .eq. name ) then
#if 0
! uncomment this to have the code give i-slices 
        do i = 1, end_index(1)
          if ( levlim .eq. -1 .or. i .eq. levlim ) then
            write(88,*)end_index(2),end_index(3),' ',trim(name),' ',k,' time ',TRIM(Datestr)
            do k = start_index(3), end_index(3)
            do j = 1, end_index(2)
                write(88,*) data(i,j,k,1)
              enddo
            enddo
          endif
        enddo
#else
! give k-slices 
        do k = start_index(3), end_index(3)
          if ( levlim .eq. -1 .or. k .eq. levlim ) then
            write(88,*)end_index(1),end_index(2),' ',trim(name),' ',k,' time ',TRIM(Datestr)
            do j = 1, end_index(2)
              do i = 1, end_index(1)
                write(88,*) data(i,j,k,1)
              enddo
            enddo
          endif
        enddo
#endif
        endif

        deallocate(data)
      endif
      call ext_ncdpar_get_next_var (dh1, VarName, Status_next_var)
      IF ( ntimes .EQ. 1 .AND. ALLOCATED(xlong) .AND. ALLOCATED(xlat) .AND. first ) THEN
        first = .FALSE.
        icenter = 1 
        jcenter = 1
        ntries = 0
        prev_icenter = 0 
        prev_jcenter = 0  
        DO WHILE ( ntries .LT. 10 .AND. (icenter .NE. prev_icenter .OR. jcenter .NE. prev_jcenter ))
          prev_icenter = icenter
          prev_jcenter = jcenter
          DO j = start_index(2), end_index(2)-1
            IF ( xlat(icenter,j) .LE. searchlat .AND. searchlat .LT. xlat(icenter,j+1) ) THEN
              jcenter = j
!write(*,*)'xlat ',ntries,icenter,jcenter,xlat(icenter,j),searchlat
              exit
            ENDIF
          ENDDO
          DO i = start_index(1), end_index(1)-1
            IF ( xlong(i,jcenter) .LE. searchlong .AND. searchlong .LT. xlong(i+1,jcenter)) THEN
              icenter = i
!write(*,*)'xlon ',ntries,icenter,jcenter,xlong(i,jcenter),searchlong
              exit
            ENDIF
          ENDDO
          ntries = ntries + 1
        ENDDO
        write(*,*)'Lon ',searchlong,' Lat ',searchlat,' : ',icenter,jcenter
        write(*,*)'Coordinates at that point ',xlong(icenter,jcenter),xlat(icenter,jcenter)
        write(*,*)'Coordinates at next point ',xlong(icenter+1,jcenter+1),xlat(icenter+1,jcenter+1)
        write(*,*)'Ntries : ',ntries
        if ( ntries .GE. 10 ) write(*,*)'max tries exceeded. Probably did not find'
      ENDIF
    enddo
    call ext_ncdpar_get_next_time(dh1, DateStr, Status_next_time)
  enddo
else
  write (6,FMT='(4A)') 'Diffing ',trim(flnm),' ',trim(flnm2)

  call ext_ncdpar_get_next_time(dh1, DateStr, Status_next_time)
  call ext_ncdpar_get_next_time(dh2, DateStr2, Status_next_time2)

  IF ( DateStr .NE. DateStr2 ) THEN
    print*,'They differ big time.  Dates do not match'
    print*,'   ',flnm,' ',DateStr
    print*,'   ',flnm2,' ',DateStr2
    Status_next_time = 1
  ENDIF

  DO WHILE ( Status_next_time .eq. 0 .AND. Status_next_time2 .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)
    print 76
    call ext_ncdpar_get_next_var (dh1, VarName, Status_next_var)
    DO WHILE ( Status_next_var .eq. 0 )
!    write(*,*)'Next Var |',TRIM(VarName),'|'

      start_index = 1
      end_index = 1
      start_index2 = 1
      end_index2 = 1

      call ext_ncdpar_get_var_info (dh1,VarName,ndim,ordering,staggering,start_index,end_index, WrfType, ierr )
      call ext_ncdpar_get_var_info (dh2,VarName,ndim2,ordering2,staggering2,start_index2,end_index2, WrfType2, ierr )
      IF ( ierr /= 0 ) THEN
        write(*,*)'Big difference: ',VarName,' not found in ',flnm2
        GOTO 1234
      ENDIF
      IF ( ndim /= ndim2 ) THEN
        write(*,*)'Big difference: Number of dimensions for ',Varname,' differs in ',flnm2,'(',ndim,') /= (',ndim2
        GOTO 1234
      ENDIF
      IF ( WrfType /= WrfType2 ) THEN
        write(*,*)'Big difference: The types do not match'
        GOTO 1234
      ENDIF
      if( WrfType == WRF_REAL) then
        DO i = 1, ndim
          IF ( end_index(i) /= end_index2(i) ) THEN
            write(*,*)'Big difference: dim ',i,' lengths differ for ',Varname,' differ in ',flnm2
            GOTO 1234
          ENDIF
        ENDDO
        DO i = ndim+1,3
          start_index(i) = 1
          end_index(i) = 1
          start_index2(i) = 1
          end_index2(i) = 1
        ENDDO

!        write(*,'(A9,1x,I1,3(1x,I3),1x,A,1x,A)')&
!                 VarName, ndim, end_index(1), end_index(2), end_index(3), &
!                 trim(ordering), trim(DateStr)

        allocate(data (end_index(1), end_index(2), end_index(3), 1))
        allocate(data2(end_index(1), end_index(2), end_index(3), 1))

        if ( ndim .eq. 3 ) then
          ord = 'XYZ'
        else if ( ndim .eq. 2 ) then
          ord = 'XY'
        else if ( ndim .eq. 1 ) then
          ord = 'Z'
        else if ( ndim .eq. 0 ) then
          ord = '0'
        endif

        call ext_ncdpar_read_field(dh1,DateStr,TRIM(VarName),data,WRF_REAL,0,0,0,ord,&
                            staggering, dimnames ,                      &
                            start_index,end_index,                      & !dom 
                            start_index,end_index,                      & !mem
                            start_index,end_index,                      & !pat
                            ierr)

        IF ( ierr /= 0 ) THEN
          write(*,*)'Error reading ',Varname,' from ',flnm
          write(*,*)'  ndim = ', ndim
          write(*,*)'  end_index(1) ',end_index(1)
          write(*,*)'  end_index(2) ',end_index(2)
          write(*,*)'  end_index(3) ',end_index(3)
        ENDIF
        call ext_ncdpar_read_field(dh2,DateStr,TRIM(VarName),data2,WRF_REAL,0,0,0,ord,&
                            staggering, dimnames ,                      &
                            start_index,end_index,                      & !dom 
                            start_index,end_index,                      & !mem
                            start_index,end_index,                      & !pat
                            ierr)
        IF ( ierr /= 0 ) THEN
          write(*,*)'Error reading ',Varname,' from ',flnm2
          write(*,*)'  ndim = ', ndim
          write(*,*)'  end_index(1) ',end_index(1)
          write(*,*)'  end_index(2) ',end_index(2)
          write(*,*)'  end_index(3) ',end_index(3)
        ENDIF

        IFDIFFS=0
        sumE = 0.0
        sum1 = 0.0
        sum2 = 0.0
        diff1 = 0.0
        diff2 = 0.0
        n = 0 
        DO K = 1,end_index(3)-start_index(3)+1
         IF (LEVLIM.EQ.-1.OR.K.EQ.LEVLIM.OR.NDIM.eq.2) THEN
          cross = 0 
          IKDIFFS = 0
          do i = 1, end_index(1)-cross
            do j = 1, end_index(2)-cross
              a = data(I,J,K,1)
              b = data2(I,J,K,1)
              ! borrowed from  Thomas Oppe's comp program
              sumE = sumE + ( a - b ) * ( a - b )
              sum1 = sum1 + a * a
              sum2 = sum2 + b * b
              diff1 = max ( diff1 , abs ( a - b ) )
              diff2 = max ( diff2 , abs ( b ) )
              n = n + 1
              IF (a .ne. b) then
                IKDIFFS = IKDIFFS + 1
                IFDIFFS = IFDIFFS + 1
              ENDIF
            ENDDO
          ENDDO
         ENDIF
        enddo
        rmsE = sqrt ( sumE / dble( n ) )
        rms1 = sqrt ( sum1 / dble( n ) )
        rms2 = sqrt ( sum2 / dble( n ) )
        serr = 0.0
        IF ( sum2 .GT. 0.0d0 ) THEN
          serr = sqrt ( sumE / sum2 )
        ELSE
          IF ( sumE .GT. 0.0d0 ) serr = 1.0
        ENDIF
        perr = 0.0
        IF ( diff2 .GT. 0.0d0 ) THEN
          perr = diff1/diff2
        ELSE
          IF ( diff1 .GT. 0.0d0 ) perr = 1.0
        ENDIF

        IF ( rms1 - rms2 .EQ. 0.0d0 ) THEN
          digits = 15
        ELSE
          IF ( rms2 .NE. 0 ) THEN
            tmp1 = 1.0d0/( ( abs( rms1 - rms2 ) ) / rms2 )
            IF ( tmp1 .NE. 0 ) THEN
              digits = log10(tmp1)
            ENDIF
          ENDIF
        ENDIF

        IF (IFDIFFS .NE. 0 ) THEN
           ! create the fort.88 and fort.98 files because regression scripts will
           ! look for these to see if there were differences.
           write(88,*)trim(varname)
           write(98,*)trim(varname)
           PRINT 77,trim(varname), IFDIFFS, ndim, rms1, rms2, digits, rmsE, perr
 76 FORMAT (5x,'Field ',2x,'Ndifs',4x,'Dims ',6x,'RMS (1)',12x,'RMS (2)',5x,'DIGITS',4x,'RMSE',5x,'pntwise max')
 77 FORMAT ( A10,1x,I9,2x,I3,1x,e18.10,1x,e18.10,1x,i3,1x,e12.4,1x,e12.4 )
        ENDIF
        deallocate(data)
        deallocate(data2)

      endif
 1234 CONTINUE
      call ext_ncdpar_get_next_var (dh1, VarName, Status_next_var)
    enddo
    call ext_ncdpar_get_next_time(dh1, DateStr, Status_next_time)
    call ext_ncdpar_get_next_time(dh2, DateStr2, Status_next_time2)
    IF ( DateStr .NE. DateStr2 ) THEN
      print*,'They differ big time.  Dates do not match'
      print*,'They differ big time.  Dates do not match'
      print*,'   ',flnm,' ',DateStr
      print*,'   ',flnm2,' ',DateStr2
      Status_next_time = 1
    ENDIF
  enddo

endif

end program readv3

logical function wrf_dm_on_monitor()
  wrf_dm_on_monitor=.true.
end function wrf_dm_on_monitor

logical function iveceq( a, b, n )
  implicit none
  integer n
  integer a(n), b(n)
  integer i
  iveceq = .true.
  do i = 1,n
    if ( a(i) .ne. b(i) ) iveceq = .false.
  enddo
  return
end function iveceq

! stubs for routines called by module_wrf_error (used by netcdf implementation of IO api)
SUBROUTINE wrf_abort
  STOP
END SUBROUTINE wrf_abort

SUBROUTINE get_current_time_string( time_str )
  CHARACTER(LEN=*), INTENT(OUT) :: time_str
  time_str = ''
END SUBROUTINE get_current_time_string

SUBROUTINE get_current_grid_name( grid_str )
  CHARACTER(LEN=*), INTENT(OUT) :: grid_str
  grid_str = ''
END SUBROUTINE get_current_grid_name

