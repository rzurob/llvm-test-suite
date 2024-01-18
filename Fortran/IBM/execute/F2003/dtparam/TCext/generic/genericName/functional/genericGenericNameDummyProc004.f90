! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/genericName/functional/genericGenericNameDummyProc004.f
! opt variations: -ql

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
!*                                             and using dummy proc to distinguish between different proc
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, pass :: setprocscalar
         procedure, pass :: setprocarray
         generic :: set => setprocscalar, setprocarray
   end type

   abstract interface
      type(base(4)) function scalarproc ()
         import base
      end function
   end interface

   abstract interface
      type(base(4)) function arrayproc ()
         import base
         allocatable :: arrayproc(:)
      end function
   end interface

   contains

      subroutine setprocscalar( a, b )
         class(base(4)), intent(inout) :: a
         procedure(scalarproc) :: b

         select type ( a )
            type is ( base(4) )
               a = b()
         end select

         print *, 'setprocscalar'

      end subroutine

      subroutine setprocarray( a, b )
         class(base(4)), intent(inout) :: a
         procedure(arrayproc) :: b

         select type ( a )
            type is ( base(4) )
               associate ( g => b() )
                  do i = 1, size(g)
                     a%i = a%i + g(i)%i
                  end do
               end associate
         end select

         print *, 'setprocarray'

      end subroutine

end module

program genericGenericNameDummyProc004
   use m

   interface
      type(base(4)) function scalarbase ()
         import base
      end function
   end interface

   interface
      type(base(4)) function arraybase()
         import base
         allocatable :: arraybase(:)
      end function
   end interface

   interface
      type(base(4)) function scalarbase1 ()
         import base
      end function
   end interface

   interface
      type(base(4)) function arraybase1 ()
         import base
         allocatable :: arraybase1(:)
      end function
   end interface

   type(base(4)), pointer :: b1
   type(base(4)), allocatable :: b2
   type(base(4)) :: b3

   allocate ( b1, b2 )

   call b1%set(scalarbase)
   print *, b1%i

   call b1%set(scalarbase1)
   print *, b1%i

   call b1%set(arraybase)
   print *, b1%i

   call b1%set(arraybase1)
   print *, b1%i

   call b2%set(scalarbase)
   print *, b2%i

   call b2%set(scalarbase1)
   print *, b2%i

   call b2%set(arraybase)
   print *, b2%i

   call b2%set(arraybase1)
   print *, b2%i

   call b3%set(scalarbase)
   print *, b3%i

   call b3%set(scalarbase1)
   print *, b3%i

   call b3%set(arraybase)
   print *, b3%i

   call b3%set(arraybase1)
   print *, b3%i

end program

type(base(4)) function scalarbase ()
   use m, only: base

   scalarbase = base(4)(100)
   print *, 'scalarbase'

end function


type(base(4)) function arraybase()
   use m, only: base
   allocatable :: arraybase(:)

   allocate ( arraybase(5), source = (/base(4)(1),base(4)(2),base(4)(3),base(4)(4),base(4)(5)/) )
   print *, 'arraybase'

end function

type(base(4)) function scalarbase1 ()
   use m, only: base

   scalarbase1=base(4)(200)
   print *, 'scalarbase1'

end function

type(base(4)) function arraybase1 ()
   use m, only: base
   allocatable :: arraybase1(:)

   allocate ( arraybase1(10), source = (/base(4)(1),base(4)(2),base(4)(3),base(4)(4),base(4)(5), base(4)(6),base(4)(7),base(4)(8),base(4)(9),base(4)(10)/) )
   print *, 'arraybase1'

end function

