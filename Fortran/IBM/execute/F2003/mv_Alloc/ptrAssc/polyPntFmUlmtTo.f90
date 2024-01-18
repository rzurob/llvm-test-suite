! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : polyPntFmUlmtTo.f 
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
!*  DESCRIPTION                : FROM is component of poly type base 
!*                               TO is of class(*), component of DT
!*                               pointer is of poly type base 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
       class(*), allocatable :: l1 
   end type

   type, extends(base) :: child 
       class(base), allocatable :: l2
    end type

end module

use m

    class(child), target,  allocatable :: C

    class(base), pointer :: p

    type(base)  B 

    B = base ( 'ABC-XYZ' )

    allocate(C, source = child(l1 =' 123456' ,l2 = B) )

    p => C%l2

    call move_alloc( C%l2, C%l1 )

    select type ( x=> C%l1)
        type is ( base)
            if ( .not. associated(p, x) ) stop 23
            select type ( y  => x%l1 )
                type is (character(*))
                    print *, y
            end select 
    end select 

end
