!*  ===================================================================
!*
!*  DATE                       : 2/25/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               dummy procedure with variable length dummy
!*                               argument.
!* ===================================================================

program mxminMisc31

    type::dt
       character(:), allocatable:: name
    end type

    character(:), pointer :: p1Char , p2Char
    character*11,  target:: tChar

    type(dt), target ::obj

    allocate(character(11):: obj%name)

    obj%name = "iBM iBM iBM"

    tChar = "I have"

    p1Char => tChar

    p2Char => obj%name

    if(len(max(p1Char, obj%name) ) .ne. 11) error stop 1_4

    if(len(min(p1Char, p2Char)) .ne. 11) error stop 2_4

    if(len(minval((/min(p1Char, obj%name), max(p1Char, obj%name)/))) .ne. 11) error stop 3_4

    call sub1(min(max(p1Char, obj%name),max(min("aa",min(p1Char, obj%name)),"ba")), minval((/min(p1Char, obj%name), max(p1Char, obj%name)/)), maxloc((/p1Char, p2Char/),dim=1))

end program mxminMisc31

  subroutine sub1 (arg1, arg2, arg3)
     character*(*) arg1, arg2
     integer arg3
     if(arg3 .ne. 2) error stop 4_4
     call sub2 ('cup        ', 'abc        ')
       contains
       subroutine sub2 (arg4, arg5)
        character*(*) arg4, arg5
        if(min(arg1, arg2, arg4, arg5) .ne. "I have     ") error stop 5_4
        if( maxval((/arg1,arg2, arg4, arg5/)) .ne. "cup        ") error stop 6_4
        if(maxloc((/arg1,arg2, arg4, arg5/),dim=1) .ne. 3) error stop 7_4
       end subroutine
  end subroutine

