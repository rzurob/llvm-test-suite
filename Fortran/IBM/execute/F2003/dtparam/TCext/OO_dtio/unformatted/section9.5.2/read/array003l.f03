! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-14 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Array entity containing unlimited polymorphic component (input)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base
      class(*), pointer :: u
   end type

   type datum (ldatum_1) ! ldatum_1=3
      integer, len :: ldatum_1
      character(ldatum_1) :: c = 'xxx'
   end type
end module

program array003l
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable     :: b1(:)
   class(base), pointer         :: b2(:,:)
   type(base)                   :: b3(2)
   type(base),  pointer         :: b4(:)
   integer(4) :: i1, i2, i3, i4
   character(3) :: c1, c2, c3, c4, c5, c6
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b1(2), b2(2,2) )

   allocate ( b1(1)%u, source = -999 )
   allocate ( b1(2)%u, source = 'xxx' )
   allocate ( b2(1,1)%u, source = datum(3)() ) ! tcx: (3)
   allocate ( b2(2,1)%u, source = 'xxx' )
   allocate ( b2(1,2)%u, source = -999 )
   allocate ( b2(2,2)%u, source = 'xxx' )
   allocate ( b3(1)%u, source = -999 )
   allocate ( b3(2)%u, source = 'xxx' )
   allocate ( b4(4) )
   allocate ( b4(1)%u, source = -999 )
   allocate ( b4(2)%u, source = 'xxx' )
   allocate ( b4(3)%u, source = -999 )
   allocate ( b4(4)%u, source = 'xxx' )

   c1 = 'abc'
   i1 = 101
   c2 = 'def'
   c3 = 'ghi'
   i2 = 102
   c4 = 'jkl'
   i3 = 103
   c5 = 'mno'
   i4 = 104
   c6 = 'pqr'

   open (unit = 1, file ='array003l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )           c1, i1
   write (1, iostat=stat, iomsg=msg )           c2, c3, i2, c4
   write (1, iostat=stat, iomsg=msg )           i3, c5
   write (1, iostat=stat, iomsg=msg )           i4, c6

   rewind 1

   read (1, iostat=stat, iomsg=msg )             b1(2:1:-1)
   read (1, iostat=stat, iomsg=msg )             b2
   read (1, iostat=stat, iomsg=msg )             b3
   read (1, iostat=stat, iomsg=msg )             b4(1:4:3)

   select type (g => b1(1)%u )
      type is (integer)
         print *,g
   end select
   select type (g => b1(2)%u )
      type is (character(*))
         print *,g
   end select
   select type (g => b2(1,1)%u )
      type is (datum(*)) ! tcx: (*)
         print *,g%c
   end select
   select type (g => b2(2,1)%u )
      type is (character(*))
         print *,g
   end select
   select type (g => b2(1,2)%u )
      type is (integer)
         print *,g
   end select
   select type (g => b2(2,2)%u )
      type is (character(*))
         print *,g
   end select
   select type (g => b3(1)%u )
      type is (integer)
         print *,g
   end select
   select type (g => b3(2)%u )
      type is (character(*))
         print *,g
   end select
   select type (g => b4(1)%u )
      type is (integer)
         print *,g
   end select
   select type (g => b4(2)%u )
      type is (character(*))
         print *,g
   end select
   select type (g => b4(3)%u )
      type is (integer)
         print *,g
   end select
   select type (g => b4(4)%u )
      type is (character(*))
         print *,g
   end select

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   interface read(unformatted)
      subroutine readUnformatteddatum (dtv, unit, iostat, iomsg)
         import datum
         class(datum(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   associate ( g => dtv%u )
   select type ( g )
      type is (integer)
         read (unit, iostat=iostat, iomsg=iomsg ) g
      type is (character(*))
         read (unit, iostat=iostat, iomsg=iomsg ) g
      type is (datum(*)) ! tcx: (*)
         read (unit, iostat=iostat, iomsg=iomsg ) g
         if ( iomsg /= 'datumread' ) error stop 101_4
   end select
   end associate

end subroutine

subroutine readUnformatteddatum (dtv, unit, iostat, iomsg)
   use m1
   class(datum(*)), intent(inout) :: dtv ! tcx: (*)
   integer,  intent(in) :: unit
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   read ( unit, iostat = iostat ) dtv%c
   iomsg = 'datumread'

end subroutine


! Extensions to introduce derived type parameters:
! type: datum - added parameters (ldatum_1) to invoke with (3) / declare with (*) - 5 changes
