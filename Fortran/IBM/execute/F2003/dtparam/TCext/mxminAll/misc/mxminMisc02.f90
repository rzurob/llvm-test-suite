! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/mxminAll/misc/mxminMisc02.f
! opt variations: -qck -qnok

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX/MIN intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : MAX*/MIN* as actual argument passed to
!*                               subprogram with variable as MAX/MIN argument 
!*
!*
!* ===================================================================

module m 
    contains
      character*20 function fc1(a)
         character*20 a
         fc1 = a
      end function fc1
end module

program mxminMisc02
use m

   interface test
      character*7 function fc2(a,b)
          character*7 a, b
      end function fc2
      module procedure fc1
   end interface test

   type dt(k1,n1)    ! (4,20)
     integer, kind :: k1
     integer, len  :: n1
     character(n1)    z
   end type 

   character(10)  x, x_arr(3)
   character*7    y, y_arr(2,2), v1, v2(2)
   character*20   v3
   integer        v4(2)

   type(dt(4,20)) :: dtobj 

   x = repeat('a', 10)
   y = "xlftest"
 
   x_arr = (/'dddddddddd', 'eeeeeeeeee' , 'zzzzzzzzzz'/) 

   y_arr = reshape(source= (/'0000000', 'absfgpw', 'AKesAAA', '_______'/)  &
             , shape = (/2,2/))

   dtobj%z = repeat('bb', 10)

   if(len(MAX(x, y, dtobj%z, 'cc', x_arr(3))) .ne. 20) then
          error stop 1_4
   endif
   
   v3 = MAX(x, y, dtobj%z, 'cc', x_arr(3))

   v1 = MAXVAL(y_arr)

   v2 = MAXVAL(y_arr, DIM=2, MASK = .true.)

   v4 = MAXLOC(y_arr, DIM=2, MASK = .true.)

   if(test(MAX(x, y, dtobj%z, 'cc', x_arr(3))) .ne. v3) then
         error stop 2_4
   endif

   if(test(MAXVAL(y_arr), MAXVAL(y_arr)) .ne. v1) then
         error stop 3_4
   endif

    call sub1(MAXLOC(y_arr, DIM=2, MASK = .true.)) 
 
    call sub2(MAXVAL(y_arr, DIM=2, MASK = .true.))

    contains
       subroutine sub1(b)
           integer b(2)
           integer a(2)
           a(1) = 2
           a(2) = 1
           if(any(a .ne. b)) then
              error stop 4_4
           endif
        end subroutine

        subroutine sub2(b)
           character*7 a(2),b(2)
           a(1) = 'AKesAAA'
           a(2) = 'absfgpw'
           if(any(a .ne. b)) then
              error stop 5_4
           endif
        end subroutine

end program mxminMisc02 

   character*7 function fc2(a, b)
       character*7 a, b
       fc2 = a 
   end function fc2

