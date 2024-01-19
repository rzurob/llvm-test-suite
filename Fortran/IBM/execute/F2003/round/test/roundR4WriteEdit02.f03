!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND with different data edit descriptor.
!*
!*  DESCRIPTION                :
!*    test different ROUND mode with different data edit descriptor with
!*    real*4. Using edit descriptor instead of ROUND= specifier.
!*    9.4.1  The modes of a connection to an external file may be changed
!*           by a subsequent OPEN statement that modifies the connection.
!* ===================================================================

  program roundR4WriteEdit02

    implicit none

    character(18) :: r_mode
    real w1, w2, w3, w4

    integer, parameter::unit = 2

    ! round in up mode

    open(unit, file='roundR4WriteEdit02.out', action='write')

    w1 = 1.250058651
    w2 = -1.250058651
    w3 = 3.141592653589
    w4 = 2.718281828459

    write(unit, '(4x,RU,f7.5, 1x, RU, f8.5)') w1, w2

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 1_4
    endif

    write(unit, '(RU, f13.6, 1x, RU, en13.6, 1x, RU, es13.6, 1x, RU,   &
     & g13.5, 1x, RU, d13.5, 1x, RU, e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 2_4
    endif

    ! round in down mode

    write(unit, '(4x,RD,f7.5, 1x, RD, f8.5)') w1, w2

    write(unit, '(RD, f13.6, 1x, RD, en13.6, 1x, RD, es13.6, 1x, RD,  &
      & g13.5, 1x, RD, d13.5, 1x, RD, e13.5)') w3, w4, w3, w4, w3, w4

    ! round in zero mode

    write(unit, '(4x,RZ, f7.5, 1x, RZ, f8.5)') w1, w2

    write(unit, '(RZ, f13.6, 1x, RZ, en13.6, 1x, RZ, es13.6, 1x, RZ,  &
      & g13.5, 1x, RZ, d13.5, 1x, RZ, e13.5)') w3, w4, w3, w4, w3, w4

    ! round in nearest mode

    write(unit, '(4x,RN, f7.5, 1x, RN, f8.5)') w1, w2

    write(unit, '(RN, f13.6, 1x, RN, en13.6, 1x, RN, es13.6, 1x, RN, &
     & g13.5, 1x, RN, d13.5, 1x, RN, e13.5)') w3, w4, w3, w4, w3, w4

    inquire(unit, round=r_mode)

    if(r_mode .ne. 'PROCESSOR_DEFINED') then
       error stop 3_4
    endif

    ! round in compatible mode

    write(unit, '(4x,RC, f7.5, 1x, RC,f8.5)') w1, w2

    write(unit, '(RC, f13.6, 1x, RC, en13.6, 1x, RC, es13.6, 1x, RC,   &
     & g13.5, 1x, RC, d13.5, 1x, RC, e13.5)') w3, w4, w3, w4, w3, w4

    ! round in processor defined mode

    write(unit, '(4x,RP, f7.5, 1x, RP, f8.5)') w1, w2

    write(unit, '(RP, f13.6, 1x, RP, en13.6, 1x, RP, es13.6, 1x, RP,   &
     & g13.5, 1x, RP, d13.5, 1x, RP, e13.5)') w3, w4, w3, w4, w3, w4

    close(unit)

  end program roundR4WriteEdit02
