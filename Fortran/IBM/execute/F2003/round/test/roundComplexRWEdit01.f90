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
!*  PRIMARY FUNCTIONS TESTED   : ROUND with READ/WRITE statement
!*                             
!*
!*  DESCRIPTION                : 
!*          test different rounding mode with external file for data
!*          input and output data transfer for complex data type.
!* ===================================================================

  program roundComplexRWEdit01 

    implicit none

    character(18) :: r_mode 
    complex(8) rd

    integer, parameter::unit_r = 2 
    integer, parameter::unit_w = 3

    rd = (0.0D0, 0.0D0)

    ! round in up mode

    open(unit_r, file='roundComplexRWEdit01.dat', action='read')
    open(unit_w, file='roundComplexRWEdit01.out', action='write')

    read(unit_r, '(RU, 2f15.13)') rd

    if(transfer(dreal(rd), 0_8) .ne. 4608308547941528973_8) error stop 1_4

    if(transfer(dimag(rd), 0_8) .ne. -4615063488913246836_8) error stop 2_4

    ! edit descriptor takes precedence over specifier in one data transfer 
    ! statement

    write(unit_w, '(a10,1x,RU,2f16.13)',round="down") "round up",rd

    rewind(unit_r)

    ! round in down mode

    read(unit_r, '(RD, 2f15.13)') rd

    if(transfer(dreal(rd), 0_8) .ne. 4608308547941528972_8) error stop 3_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246835_8) error stop 4_4

    write(unit_w, '(a10,1x,RD,2f16.13)', round="up") "round down",rd

   rewind(unit_r)

    ! round in zero mode

    read(unit_r, '(RZ, 2f15.13)') rd

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 5_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 6_4

    write(unit_w, '(a10,1x,RZ,2f16.13)', round="up") "round zero",rd

   rewind(unit_r)

    ! round in nearest mode

    read(unit_r, '(RN, 2f15.13)') rd

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 7_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 8_4

    write(unit_w, '(a13,1x,RN,2f16.13)', round="up") "round nearest",rd

   rewind(unit_r)

    ! round in processor_defined mode

    read(unit_r, '(RP, 2f15.13)') rd

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 9_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 10_4

    write(unit_w, '(a23,1x,RP,2f16.13)', round="up")                    &
       & "round processor_defined", rd

   rewind(unit_r)

    ! round in compatible mode

    read(unit_r, '(RC, 2f15.13)') rd

    if(transfer(dreal(rd),0_8) .ne. 4608308547941528972_8) error stop 11_4

    if(transfer(dimag(rd),0_8) .ne. -4615063488913246836_8) error stop 12_4

    write(unit_w, '(a23,1x,RC,2f16.13)', round="up") "round compatible",rd

    close(unit_r)
    close(unit_w)

  end program roundComplexRWEdit01 
