!*  ===================================================================
!*
!*  DATE                       : 1/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MAX/MIN with scalar substring as its argument
!*
!*                               inherited length with max/min as expression
!*                               in character relational expressions
!* ===================================================================

  program mxminScalarArgSubExpression

    character*20 x, y
    parameter(x = "iamworkinginIBMhhhhh")
    parameter(y = "totallengthistwienty")

    call sub1(max(x(1:10), y(1:10)), min(x(1:10), y(1:10)))

    contains

        subroutine sub1(arg1, arg2)
          character*(*) arg1, arg2
          character*20 arg3
          arg3 = max(arg1, arg2) // min(arg1, arg2)
          if(arg3 .ne. "totallengtiamworking") then
               error stop 1_4
          endif
        end subroutine

  end program mxminScalarArgSubExpression

