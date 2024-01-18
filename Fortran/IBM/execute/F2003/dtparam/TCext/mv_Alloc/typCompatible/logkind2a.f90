! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/mv_Alloc/typCompatible/logkind2a.f
! opt variations: -qnok -ql

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : logkind2a.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM is of logical*2, a component of DT
!*                              TO is of unlimit poly, a component of DT
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

          
module m
   type base(k1)    ! (2)
       integer, kind            :: k1
       logical(k1), allocatable :: ll(:)
   end type

   type class(k2)    ! (4)
       integer, kind :: k2
       class(*), allocatable ::  ul(:)
   end type

   contains
       logical function func()
           logical, allocatable :: a
           func = allocated(a)
       end function

end module

program main

   use m
       type(base(2)), allocatable :: b1
       type(class(4)) :: c1

       logical(2), allocatable :: mll(:)

       call sub( 5 )

       if ( allocated(b1%ll) ) stop 11
       if ( .not. allocated(c1%ul) ) stop 13

       select type ( x => c1%ul)
           type is (logical(2))
                if ( size(x) /= 2) stop 21
                if ( x(1) .neqv. .true. ) stop 23
                if ( x(2) .neqv. .false. ) stop 25
           class default
                stop 32
        end select

   contains
        subroutine sub( a )
           integer, optional :: a
           allocate(mll(2), source = (/ logical( present(a), 2) ,&
                             logical ( func(), 2) /) )

           allocate(b1, source = base(2)(mll))

           call move_alloc(b1%ll,  c1%ul ) 
           
        end subroutine

end program
