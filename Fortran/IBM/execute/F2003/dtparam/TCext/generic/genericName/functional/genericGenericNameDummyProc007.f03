! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameDummyProc007.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
!*                                             and using dummy proc with array dummy arg and func result
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

   type base(n1,k1)    ! (20,4)
      integer, kind            :: k1
      integer, len             :: n1
      integer(k1), allocatable :: i(:)
      contains
         procedure, pass :: setbasewbase
         procedure, pass :: setbasewint

         generic :: set => setbasewbase, setbasewint

   end type

   abstract interface
      integer(4) function inttoint(i)
         integer(4), intent(in) :: i(:)
         allocatable :: inttoint(:)
      end function
   end interface

   abstract interface
      type(base(20,4)) function inttobase(i)
         import base
         integer(4), intent(in) :: i(:)
      end function
   end interface

   contains

      subroutine setbasewbase( a, b, c )
         class(base(*,4)), intent(inout) :: a
         procedure(inttobase) :: b
         integer, intent(in) :: c(:)

         print *, 'setbasewbase'

         if ( allocated( a%i ) ) deallocate ( a%i )
         allocate ( a%i(size(c)) )

         select type ( a )
            type is ( base(*,4) )
               a = b(c)
         end select

      end subroutine

      subroutine setbasewint( a, b, c )
         class(base(*,4)), intent(inout) :: a
         procedure(inttoint) :: b
         integer, intent(in) :: c(:)

         print *, 'setbasewint'

         if ( allocated( a%i ) ) deallocate ( a%i )
         allocate ( a%i(size(c)) )

         a%i = b(c)

      end subroutine

end module

program genericGenericNameDummyProc007
   use m

   interface
      integer(4) function returnintplusplus(i)
         integer(4), intent(in) :: i(:)
         allocatable :: returnintplusplus(:)
      end function
   end interface

   interface
      integer(4) function returnintminusminus(i)
         integer(4), intent(in) :: i(:)
         allocatable :: returnintminusminus(:)
      end function
   end interface

   interface
      type(base(20,4)) function returninttobase(i)
         import base
         integer(4), intent(in) :: i(:)
      end function
   end interface

   interface
      type(base(20,4)) function returndoublebase(i)
         import base
         integer(4), intent(in) :: i(:)
      end function
   end interface

   type(base(20,4)) :: b1
   class(base(:,4)), allocatable :: b2
   type(base(:,4)), pointer :: b3

   allocate ( base(20,4):: b2, b3 )

   call b1%set(returnintplusplus, (/1,2,3,4,5/) )
   print *, b1%i
   call b1%set(returnintminusminus, (/0,1,b1%i/) )
   print *, b1%i
   call b1%set(returninttobase, (/1,2,3,4,5/) )
   print *, b1%i
   call b1%set(returndoublebase, (/b1%i,6,7,8/) )
   print *, b1%i

   call b2%set(returnintplusplus, (/1,2,3,4,5/) )
   print *, b2%i
   call b2%set(returnintminusminus, (/0,1,b2%i/) )
   print *, b2%i
   call b2%set(returninttobase, (/1,2,3,4,5/) )
   print *, b2%i
   call b2%set(returndoublebase, (/b2%i,6,7,8/) )
   print *, b2%i

   call b3%set(returnintplusplus, (/1,2,3,4,5/) )
   print *, b3%i
   call b3%set(returnintminusminus, (/0,1,b3%i/) )
   print *, b3%i
   call b3%set(returninttobase, (/1,2,3,4,5/) )
   print *, b3%i
   call b3%set(returndoublebase, (/b3%i,6,7,8/) )
   print *, b3%i

end program

integer(4) function returnintplusplus(i)
   integer(4), intent(in) :: i(:)
   allocatable :: returnintplusplus(:)

   allocate ( returnintplusplus(size(i)) )
   returnintplusplus = i + 1

   print *, 'returnintplusplus'

end function

integer(4) function returnintminusminus(i)
   integer(4), intent(in) :: i(:)
   allocatable :: returnintminusminus(:)

   allocate ( returnintminusminus(size(i)) )
   returnintminusminus = i - 1

   print *, 'returnintminusminus'
end function

type(base(20,4)) function returninttobase(i)
   use m, only: base
   integer(4), intent(in) :: i(:)

   allocate ( returninttobase%i(size(i)), source = i )

   print *, 'returninttobase'

end function

type(base(20,4)) function returndoublebase(i)
   use m, only: base
   integer(4), intent(in) :: i(:)

   allocate ( returndoublebase%i(size(i)), source = i*2 )

   print *, 'returndoublebase'

end function
