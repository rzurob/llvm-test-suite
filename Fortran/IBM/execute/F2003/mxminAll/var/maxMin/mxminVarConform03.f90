!*  ===================================================================
!*
!*  DATE                       : 1/15/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TC to test derived type component as argument
!*                               to max/min with different length.
!*                               one argument is scalar.
!*                               use substring as max/min argument
!*                               use array section as max/min argument
!*                               use vector subscript
!* ===================================================================

  program mxminVarConform03

     type dt
        character(len=3, kind=1) x, x3(12), y3(12)
        character(len=8, kind=1) y(2,4)
     end type

     character*10       x1, y1

     integer, dimension(12) :: vector = (/1,1,1,1,1,1,1,1,2,3,4,4/)

     type(dt) :: dt_object

     dt_object%x = "ddd"
     dt_object%y = "sssskkkk"

     x1 = "abc   fddd"
     y1 = "dxydd     "

     dt_object%x3 = "KGB"
     dt_object%y3 = "IBM"

     if(len(max(dt_object%x, dt_object%y)) .ne. 8)  error stop 1_4

     if(len(min(dt_object%x, dt_object%y)) .ne. 8) error stop 2_4

     if(any(max(min(dt_object%x,dt_object%y)(1:2), max(dt_object%x, dt_object%y)(5:7)) .ne. "kkk")) then
        error stop 3_4
     endif

     if(any(min(dt_object%x, dt_object%y) .ne. "ddd     ")) error stop 4_4

     if(len(max(x1(1:5), y1(6:10))) .ne. 5)  error stop 5_4

     if(max(x1(1:5), y1(6:10)) .ne. "abc  ") error stop 6_4

     if(len(min(dt_object%x3(1:5), dt_object%y3(1:5))) .ne. 3) error stop 7_4

     if(any(min(dt_object%x3(1:5), dt_object%y3(1:5)) .ne. "IBM")) error stop 8_4

     if(any(max(dt_object%x3(1:5), dt_object%y3((/1,1,1,1,1/))) .ne. "KGB")) error stop 9_4

     if(any(min(dt_object%x3(vector(9:12)), dt_object%y3(1:4)) .ne. "IBM")) error stop 10_4

  end program mxminVarConform03

