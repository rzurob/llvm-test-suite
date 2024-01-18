!***********************************************************************
!* =================================================================== *
!*                                                                     *
!* DATE                       : May 13, 2008                           *
!*                                                                     *
!* DESCRIPTION                : C527: If the VALUE attribute is        *
!*         specified, the DIMENSION attribute shall not be specified.  *
!*                                                                     *
!* STRUCTURE                  : MAIN                                   *
!*                                                                     *
!* EXECUTABLE                 : No                                     *
!*                                                                     *
!* DEPENDENCIES               : None                                   *
!*                                                                     *
!* REQUIRED COMPILER OPTIONS  : None                                   *
!*                                                                     *
!* NORMAL COMPLETION          : Return code = 1                        *
!*                                                                     *
!* ABNORMAL COMPLETION        : Return code = 0                        *
!*                                                                     *
!* RUN TIME ESTIMATE          : <60 SEC                                *
!*                                                                     *
!* CONDITIONS TESTED          : Listed below.                          *
!***********************************************************************

      ! C527: If the VALUE attribute is specified, the PARAMETER, EXTERNAL,
      ! POINTER, ALLOCATABLE, DIMENSION, VOLATILE, INTENT(INOUT), or
      ! INTENT(OUT) attribute shall not be specified.
       module m
         type dt(l)
           integer, len :: l = 2
           integer i(l-1)
         end type
       end module

       subroutine sub(a,b1,b2,b3,c1,c2,c3,d1,d2,d3,d4,d5,n)
         use m
         integer :: i = n

         dimension :: b2(4,2), d2(4,2), d3(4,2)
         value :: c2, d2, d4
         type(dt), value, dimension(:,:) :: a
         type(dt), value :: b1, b2, b3
         type(dt), dimension(2,8) :: c1, c2, c3
         type(dt) :: d1, d2, d3, d4, d5
         value :: c3, d3, d5
         dimension :: b3(:,:), d4(n,i), d5(8,3)

       end subroutine
