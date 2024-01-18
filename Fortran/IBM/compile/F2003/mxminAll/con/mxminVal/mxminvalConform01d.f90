!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 1/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 13.7.71[3,4,6,8,9]:
!*                               character argument for MAX*/MIN* intrinsics 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*
!*  DESCRIPTION                : Diagnostic TC for maxval/minval
!*
!* ===================================================================

  program mxminvalConform01d 

     interface
        subroutine sub1(c1arg, c2arg)
            character*4, dimension(4) :: c1arg, c2arg
            optional c2arg
        end subroutine
     end interface

     character*4 x1, y1(4), y3(4)
     integer     x2, y2(4) 
     logical     z1(2,2)
     
     parameter(x1 = "abcd")
     parameter(y1 = "dbca")

     parameter(y3 = "sdfg")

     parameter(x2 = 1)
     parameter(y2 = 1)

     parameter(z1 = .true.)

      call sub1(y1) 

     print *, maxval(y1, dim = y2)

     print *, maxval(y1, dim = y2, mask = .true.)

     print *, minval(x1)

     print *, maxval(x1, dim=1, mask=.false.)

     print *, minval(y1, dim=1, mask=z1)

 end program mxminvalConform01d

     subroutine sub1(c1arg, c2arg)
       character*4, dimension(4) :: c1arg, c2arg
       optional c2arg
       print *, maxval(c2arg)       

     end subroutine
