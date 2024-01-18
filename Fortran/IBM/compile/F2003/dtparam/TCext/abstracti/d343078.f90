!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d343078
!*
!*  PROGRAMMER                 : Glen Mateer (derived from abstracti014ckl)
!*
!*  DATE                       : 2007-10-24
!*
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Abstract Interface
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DEFECT ABSTRACT            : DTPARAM:ICE:Syntax Error in ABSTRACT INTERFACE
!*                               FUNCTION Return
!*
!*  DESCRIPTION                :
!*  The Reduced Code (below) contains a syntax error in the definition of
!*  the FUNCTION Return for "cloneInterface()" (an extra Type Parameter),
!*  which error causes the compiler to ICE.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    type, abstract :: AbstractParent(k1)    ! (4,20)
        integer, kind :: k1
        contains
        procedure(cloneInterface), pass, deferred :: clone
    end type

    abstract interface
        function cloneInterface(a)
            import :: AbstractParent
            class(AbstractParent(4)), intent(in) :: a
            class(AbstractParent(4,20)), allocatable :: cloneInterface ! <- Causes ICE
        end function
    end interface

end module
