! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpoly2.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 05/24/2006
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
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               FROM has protected, target attritbue 
!*                               TO has private, target attritbue 
!*                               derived-type, complex
!*                        
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type T 
    end type

    class(*), protected, target, allocatable :: old

    class(*), private, target, allocatable ::  new
    contains
        subroutine sub

            allocate(old, source = (20.1_4, 40.1_4))
    
            if ( .not. allocated(old) ) stop 11

            allocate ( T :: new)

            if ( .not. allocated(new) ) stop 12

            call move_alloc(old,new)

            if ( allocated(old) ) stop 21

            if ( .not. allocated(new) ) stop 22

            select type(new)
                 type is (complex)
                    write (*, '("(",f10.2,", ", f10.2, ")")') new 
                 class default
                    stop 23 
            end select

        end subroutine
end module

program main
use m
    call sub
end
