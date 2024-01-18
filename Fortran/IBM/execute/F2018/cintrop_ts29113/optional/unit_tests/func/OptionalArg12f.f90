!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : OptionalArg12f
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : May 23, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the actual argument is an optional
!*                               argument with the pointer or allocatable
!*                               attributes and the dummy argument of the
!*                               BIND(C) procedure is non-pointer, non-
!*                               allocatable optional argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine x(arg) bind(C)
           import c_int
           integer(c_int), optional :: arg
         end
      end interface

      integer(c_int), pointer :: ptr
      integer(c_int), target :: targ
      integer(c_int), allocatable :: al
      
      ptr => targ
      call y(ptr)
      call y()
      call y(ptr)

      nullify(ptr)
      call y(ptr)

      allocate(al)
      call z(al)
      call z()
      call z(al)

      deallocate(al)
      call z(al)

      contains
      subroutine y(a)
        integer(c_int), optional, pointer :: a
        print *, "y::present?", present(a)
        if (present(a)) then
           print *, "y::associated?", associated(a)
           call x(a)
        end if
      end

      subroutine z(a)
        integer(c_int), optional, allocatable :: a
        print *, "z::present?", present(a)
        if (present(a)) then
           print *, "z::allocated?", allocated(a)
           call x(a)
        end if
      end

      end


      subroutine x(a) bind(C)
        use, intrinsic :: iso_c_binding
        integer(c_int), optional :: a
        
        print *, "  x::present?", present(a)
      end
