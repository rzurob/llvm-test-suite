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
!*                               - Unlimited polymorphic array entity containing unlimited polymorphic component (input)
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

program array003al
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
   class(*), allocatable     :: b1(:)
   class(*), pointer         :: b2(:,:)

   integer(4) :: i1, i2
   character(3) :: c1, c2, c3, c4
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( base :: b1(2), b2(2,2) )

   c1 = 'abc'
   i1 = 101
   c2 = 'def'
   c3 = 'ghi'
   i2 = 102
   c4 = 'jkl'

   select type ( b1 )
      type is (base)
         allocate ( b1(1)%u, source = -999 )      !<- integer type
         allocate ( b1(2)%u, source = 'xxx' )     !<- character type
      class default
         error stop 101_4
   end select

   select type ( b2 )
      type is (base)
         allocate ( b2(1,1)%u, source = datum(3)() ) !<- datum type ! tcx: (3)
         allocate ( b2(2,1)%u, source = 'xxx' )   !<- character type
         allocate ( b2(1,2)%u, source = -999 )    !<- integer type
         allocate ( b2(2,2)%u, source = 'xxx' )   !<- character type
      class default
         error stop 2_4
   end select

   open (unit = 1, file ='array003al.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )              c1, i1
   write (1, iostat=stat, iomsg=msg )              c2, c3, i2, c4

   rewind 1

   select type ( b1 )
      class is (base)
         read (1, iostat=stat, iomsg=msg )          b1(2:1:-1)
      select type ( g => b2 )
         class is (base)
            read (1, iostat=stat, iomsg=msg )       g
      end select
   end select

   select type ( b1 )
      class is (base)
         select type ( b => b1(1)%u )
            type is (integer)
               print *, b
         end select
         select type ( b => b1(2)%u )
            type is (character(*))
               print *, b
         end select
   end select


   select type ( b2)
      type is (base)
         select type ( b => b2(1,1)%u )
            type is (datum(*)) ! tcx: (*)
               print *, b
         end select
         select type ( b => b2(2,1)%u )
            type is (character(*))
               print *, b
         end select
         select type ( b => b2(1,2)%u )
            type is (integer)
               print *, b
         end select
         select type ( b => b2(2,2)%u )
            type is (character(*))
               print *, b
         end select
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
         if ( iomsg /= 'datumread' ) error stop 5_4
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
