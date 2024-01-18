! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/genericName/functional/genericGenericNameOptional004.f
! opt variations: -qck -qnol -qnodeferredlp -qreuse=base

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
!*  DESCRIPTION                : generic-name: optional dummy arguments with class hierarhcy
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

   type :: base(n1,k1)    ! (3,4)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c
      integer(k1)   :: i
      contains
         procedure, pass :: optionalchar
         procedure, pass :: optionalint
         generic :: set => optionalchar, optionalint

         procedure, pass :: writebase
         generic :: write(formatted) => writebase
   end type

   type, extends(base) :: child(n2,k2)    ! (3,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: j = 0
      contains
         generic :: set => optionalintint
         procedure, pass(b) :: optionalintint
   end type

   contains

      subroutine writebase( dtv, unit, iotype, v_list, iostat, iomsg )
         class(base(*,4)), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, dimension(:), intent(in) :: v_list
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base(*,4) )
               write ( unit, "(1x,A3,':',I5)", iostat = iostat, iomsg = iomsg ) dtv%c, dtv%i
            type is ( child(*,4,*,4) )
               write ( unit, "(1x,A3,':',I5, I5)", iostat = iostat, iomsg = iomsg ) dtv%c, dtv%i, dtv%j
         end select

      end subroutine

      subroutine optionalchar ( a, b, c )
         class(base(*,4)), intent(inout) :: a
         character(3), intent(in) :: b
         integer(4), intent(in), optional :: c

         a%c = b

         if ( present(c) ) a%i = c

         print *, 'optionalchar'

      end subroutine

      subroutine optionalint ( a, b, c )
         class(base(*,4)), intent(inout) :: a
         integer(4), intent(in) :: b
         character(3), intent(in), optional :: c

         a%i = b

         if ( present(c) ) a%c = c

         print *, 'optionalint'

      end subroutine

      subroutine optionalintint ( a, b, c, d )
         class(child(*,4,*,4)), intent(inout) :: b
         integer(4), intent(in) :: a, c
         character(3), intent(in), optional :: d

         b%i = a
         b%j = c

         if ( present(d) ) b%c = d

         print *, 'optionalintint'

      end subroutine

end module

program genericGenericNameOptional004
   use m

   class(base(:,4)), allocatable :: b1, b2
   type(child(3,4,20,4)) :: c1

   character(3), parameter :: cc1 = 'ibm'
   integer, parameter :: YEAR = 2006

   allocate ( base(20,4):: b1, b2 )

   call b1%set(10)
   call b1%set('abc')

   call b2%set(YEAR)
   call b2%set(cc1)

   print *, b1
   print *, b2

   call b1%set(YEAR,cc1)
   call b2%set('xlf',2003)

   print *, b1
   print *, b2

   c1%j = 0
   call c1%set(YEAR)
   call c1%set('FTN')
   print *, c1

   call c1%set(1234,5678)
   print *, c1

   call c1%set(100,200,'xyz')
   print *, c1

   deallocate ( b1, b2 )
   allocate ( child(3,4,20,4) :: b1, b2 )

   select type ( b1 )
      type is ( child(*,4,*,4) )
         call b1%set(1001,2001,'ibm')
         print *, b1
   end select

   select type ( b2 )
      type is ( child(*,4,*,4) )
         call b2%set('ibm',10000)
         print *, b2
         call b2%set(1,2,'abc')
         print *, b2
   end select

end program
