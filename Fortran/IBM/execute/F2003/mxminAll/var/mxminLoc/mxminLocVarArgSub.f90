!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 2/25/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : argument of MAXLOC/MINLOC is deferred character
!*                               length with pointer attribute.
!* ===================================================================

  program mxminLocVarArgSub 

    type::dt
       character(:), allocatable:: name
    end type

    character(:), pointer :: p1Char , p2Char
    character*11,  target:: tChar 

    type(dt), target ::obj

    allocate(character(11):: obj%name)

    obj%name = "iBM iBM iBM"

    tChar = "I have     "

    p1Char => tChar

    p2Char => obj%name 

    if(maxloc((/p1Char, p2Char/),dim=1) .ne. 2) error stop 1_2

    if(minloc((/p1Char, p2Char/),dim=1)  .ne. 1) error stop 2_2

    call sub1(maxval((/p1Char, p2Char/)))
  
  end program mxminLocVarArgSub 

  subroutine sub1 (arg1)
     character*(*) arg1 
     call sub2 ('cup        ')
       contains
       subroutine sub2 (arg2)
         character*(*) arg2 
         if(minloc((/arg1, arg2/),dim=1, mask=.true.) .ne. 2) error stop 3_4
         if(maxloc((/arg1, arg2/),dim=1, mask=.true.) .ne. 1) error stop 4_4
        end subroutine
  end subroutine


