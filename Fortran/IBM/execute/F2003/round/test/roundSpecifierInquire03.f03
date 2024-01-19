!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier= in inquire
!*
!*  DESCRIPTION                : using substring for variable of
!*                               round= specifier in inquire
!* ===================================================================

  program roundSpecifierInquire03

     character(:), allocatable :: r_mode(:)
     allocate (character*18 :: r_mode(6))

     r_mode=repeat(' ', 18)

     open(10, file='file1', round="up")
     open(11, file='file2', round="down")
     open(12, file='file3', round="zero")
     open(13, file='file4', round="nearest")
     open(14, file='file5', round="compatible")
     open(15, file='file6', round="PROCESSOR_DEFINED")

     do i =1, 6
        inquire(i+9, round=r_mode(i)(i+8:))
     end do

     if(r_mode(1) .ne. '        UP        ') error stop 1_4
     if(r_mode(2) .ne. '         DOWN     ') error stop 2_4
     if(r_mode(3) .ne. '          ZERO    ') error stop 3_4
     if(r_mode(4) .ne. '           NEAREST') error stop 4_4
     if(r_mode(5) .ne. '            COMPAT') error stop 5_4
     if(r_mode(6) .ne. '             PROCE') error stop 6_4

  end program roundSpecifierInquire03
