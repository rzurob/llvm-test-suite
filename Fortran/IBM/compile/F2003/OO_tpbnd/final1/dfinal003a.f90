!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: dcomp dfinal003a.f
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dfinal003a.f
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : final subroutines 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DESCRIPTION                : testing final subroutines: a final-
!*                               subroutine-name shall be the name of
!*                               a module procedure with exactly one
!*                               dummy argument.
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: x
        contains
        final :: finalizeBase !* expect an error message here
    end type
    
    type, extends(base) :: child
    contains
       final :: finalizeChild !no error message here since parent is already in error
    end type

    contains
    subroutine finalizeBase (b1, b2)
       type(base) :: b1
       class(base) :: b2
       print *, 'finalizeBase'
    end subroutine
    subroutine finalizeChild (b1, b2, b3)
       type(child) :: b1
       class(child) :: b2, b3
       print *, 'finalizeChild'
    end subroutine
end module
end
