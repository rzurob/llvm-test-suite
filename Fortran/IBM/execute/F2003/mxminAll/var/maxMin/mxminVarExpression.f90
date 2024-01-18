!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with variable substring as its argument
!*
!*                               inherited length with max/min as expression
!*                               in character relational expressions
!* ===================================================================

  program mxminVarExpression

    character*20 x, y
    character*10 x1(20), y1(20)
    x = "iamworkinginIBMhhhhh"
    y = "totallengthistwienty"

    x1 = "iamworkinginIBMhhhhh"
    y1 = "totallengthistwienty"

    call sub1(max(x(1:10), y(1:10)), min(x(1:10), y(1:10)))

    call sub1(max(x1(1), y1(1)), min(x1(1), y1(1)))

    call sub2(max(x1(1:10), y1(1:10)), min(x1(1:10), y1(1:10)))

    contains

        subroutine sub1(arg1, arg2)
          character*(*) arg1, arg2
          character*20 arg3
          arg3 = max(arg1, arg2) // min(arg1, arg2)
          if(arg3 .ne. "totallengtiamworking") then
               error stop 1_4
          endif
        end subroutine

        subroutine sub2(arg1, arg2)
          character*(*), dimension(*):: arg1, arg2
          character*20, dimension(10) :: arg3
          arg3 = max(arg1(1:10), arg2(1:10)) // min(arg1(1:10), arg2(1:10))
          if(any(arg3 .ne. "totallengtiamworking")) then
               error stop 2_4
          endif
        end subroutine

  end program mxminVarExpression

