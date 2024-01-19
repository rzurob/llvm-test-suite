!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier=
!*
!*  DESCRIPTION                : test character expression is array
!*                               or character concatenation.
!*
!* ===================================================================

  program roundSpecifierOpen01

    character*17 r_mode(6), rMode
    character*17 round(6)

    r_mode=repeat('', 6)

    round = (/'UP               ', 'DOWN             ',           &
              'ZERO             ', 'NEAREST          ',           &
              'COMPATIBLE       ', 'PROCESSOR_DEFINED'/)

    open(11, file="tstRound.dat", round='U'//'P') !<- character concatenation

    inquire(11, round=rMode)

    if(rMode .ne. 'UP') error stop 1_4

    do i = 1, 6
      open(12, file="tstRoundAgain", round=round(i)) !<--using array
      inquire(12, round=r_mode(i))
    end do

   do i=1,6
     if(r_mode(i) .ne. round(i)) call zzrc(i+1_4)
   end do

  end program roundSpecifierOpen01
