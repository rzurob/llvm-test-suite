! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : unlimitpoly3.f
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 05/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC(FROM, TO) 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM and TO are unlimit polymorphic,
!*                               FROM has private attribute, TO has public attr 
!*                               type real 
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
    class(*), private, allocatable :: old
    class(*), public, allocatable ::  new
  
    contains
        subroutine sub

            allocate(old, source = 10.8)
    
            if ( .not. allocated(old) ) stop 11

            call move_alloc(old,new)

            if ( allocated(old) ) stop 12
        end subroutine
end module

program main
use m

    allocate(new, source="FORTRAN")
           
    call sub

    if ( .not. allocated(new) ) stop 22

    select type(new)
        type is (real)
            if ( new /= 10.8) stop 23
        class default
            stop 23 
    end select
end
