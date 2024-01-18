! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c13h bind_c01c
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_13h.f
!* TEST CASE TITLE              : BIND(C) attribute
!*
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Sep. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute with allocatable
!*                                array of different data types  
!*                                Using external subroutine,interface.
!*                                Fortran calls C.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxbind_c01c
   implicit none

   interface
                   
       subroutine extsub_arr1(a, b) bind(c) 
         integer a(3,2,1)
         real    b(3,2,1)
       end subroutine extsub_arr1
       
       subroutine extsub_arr2(c, l) bind(c) 
         character*1 c(3,2,1)
         logical*1 l(3,2,1)
       end subroutine extsub_arr2
       
       subroutine extsub_arr3(x) bind(c) 
         complex x(3,2,1)
       end subroutine extsub_arr3
       
    end interface

        logical precision_R4, precision_R6, precision_R8
        logical precision_x8

!**********************************************************
! Vaviable initialization. 
!**********************************************************
        
        integer, allocatable ::  a(:,:,:)
        real, allocatable ::    b(:,:,:)
        
        logical*1, allocatable :: l(:,:,:)
        character*1, allocatable  ::  c(:,:,:)
        
        complex(4), allocatable :: x(:,:,:)
        
        integer i /3/ 
        integer j /2/
        
        allocate(a(3,2,1), b(3,2,1), c(i,j,1),l(3,2,1), x(3,2,1))
        l = .false.

!**********************************************************
!Calling external subroutine with array of complex
!**********************************************************

          call extsub_arr3(x)
          do i = 1, 2
             do j = 1, 3
               if ( .not.precision_x8(x(j,i,1),(1.0,3.0)) ) then  
                 error stop 31
               end if
             end do
          end do
           

!**********************************************************
!    Calling external subroutine with array of character and
!    locical data type which will call C and check the results
!**********************************************************

           call extsub_arr2(c, l)
           if ( any(c .ne. 'a')) error stop 20
           if ( any(l .neqv. .true.)) error stop 21
          

!**********************************************************
!   Calling external subroutine with array of integer and real 
!         which will call C and check the results
!**********************************************************

           call extsub_arr1(a,b)
           if( any(a .ne. 3)) error stop 10 
           if( any(b .ne. 3.4)) error stop 11
 

end 
