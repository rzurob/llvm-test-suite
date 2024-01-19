! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mxminAll/var/maxMin/mxminVarArgSub1.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 2/25/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument of MAX/MIN is deferred character
!*                               length with pointer attribute.
!* ===================================================================

  program mxminVarArgSub1

    type::dt(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
       character(:), allocatable:: name
    end type

    character(:), pointer :: p1Char , p2Char
    character*6,  target:: tChar

    type(dt(4,20)), target ::obj

    allocate(character(11):: obj%name)

    obj%name = "iBM iBM iBM"

    tChar = "I have"

    p1Char => tChar

    p2Char => obj%name

    if(len(max(p1Char, obj%name) ) .ne. 11) error stop 1_4

    if(len(min(p1Char, p2Char)) .ne. 11) error stop 2_4

    call sub1(max(max(p1Char, obj%name), min(p1Char, p2Char), "aa", "ab"))

  end program mxminVarArgSub1

  subroutine sub1 (arg1)
     character*(*) arg1
     call sub2 ('cup')
       contains
       subroutine sub2 (arg2)
         character*(*) arg2
         if(len(arg1 //  arg2 ) .ne. 14) error stop 3_4
         if(max(arg1//arg2, "aa") .ne. "iBM iBM iBMcup") error stop 4_4
        end subroutine
  end subroutine

