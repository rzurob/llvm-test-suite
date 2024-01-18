!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 24/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*                             
!*
!*  DESCRIPTION                : 
!*                           test both round specifier and descriptor
!*                           specified. There should be no side-effect.
!* ===================================================================

  program roundX4WriteEdit02a 

    implicit none
 
    character(18) :: r_mode 
    complex w1, w2

    integer, parameter::unit = 2 

    ! round in up mode

    open(unit, file='roundX4WriteEdit02a.out', action='write', round="up")

    w1 = (1.250058651, -1.250058651)
    w2 = (3.141592653589, 2.718281828459)

    write(unit, '(1x, RU,2f10.5, 1x, 2f15.6)', round="up") w1, w2

    write(unit, '(1x, RU, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
      & 2e14.6)', round="up") w1, w2, w1, w2, w1

    ! round in down mode

    open(unit, file='roundX4WriteEdit02a.out', action='write', round="down")

    write(unit, '(1x,RD, 2f10.5, 1x, 2f15.6)', round="down") w1, w2

    write(unit, '(1x, RD,2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x,  &
     & 2e14.6)', round="down") w1, w2, w1, w2, w1

    ! round in zero mode

    open(unit, file='roundX4WriteEdit02a.out', action='write', round="zero")

    write(unit, '(1x,RZ,2f10.5, 1x, 2f15.6)', round="zero") w1, w2

    write(unit, '(1x, RZ, 2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
      & 2e14.6)', round="zero") w1, w2, w1, w2, w1

    ! round in nearest mode

    open(unit,file='roundX4WriteEdit02a.out',action='write',round="nearest")

    write(unit, '(1x,RN,2f10.5, 1x, 2f15.6)', round="nearest") w1, w2

    write(unit, '(1x,RN,2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
      & 2e14.6)', round="nearest") w1, w2, w1, w2, w1

    ! round in compatible mode

    open(unit, file='roundX4WriteEdit02a.out', action='write',           &
      & round="compatible")

    write(unit, '(1x, RC, 2f10.5, 1x, 2f15.6)', round="compatible") w1, w2

    write(unit, '(1x, RC,2en13.5, 1x, 2es15.6, 1x, 2g13.6, 1x, 2d15.7, 1x, &
       & 2e14.6)', round="compatible") w1, w2, w1, w2, w1

    ! round in processor defined mode

    open(unit, file='roundX4WriteEdit02a.out', action='write',           &
       & round="processor_defined")

    write(unit, '(1x,RP, 2f10.5, 1x, 2f15.6)', round="processor_defined")&
       &  w1, w2

    write(unit, '(1x,RP,2en13.5, 1x,2es15.6, 1x,2g13.6,1x,2d15.7, 1x,   &
       & 2e14.6)', round="up") w1, w2, w1, w2, w1

    ! round specifier or descriptor is not specified

    open(unit, file='roundX4WriteEdit02a.out', action='write')

    write(unit, '(1x,RP,2f10.5, 1x, 2f15.6)') w1, w2

    write(unit, '(1x, RP,2en13.5, 1x,2es15.6,1x,2g13.6,1x,2d15.7, 1x,  &
      & 2e14.6)') w1, w2, w1, w2, w1

   close(unit)

  end program roundX4WriteEdit02a 
