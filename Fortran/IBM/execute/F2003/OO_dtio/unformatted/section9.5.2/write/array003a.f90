!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array003a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Unlimited polymorphic array entity containing unlimited polymorphic component (output)
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

   type datum
      character(3) :: c = 'ibm'
   end type
end module

program array003a
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(*), allocatable     :: b1(:)
   class(*), pointer         :: b2(:,:)

   integer(4) :: i1, i2, i3, i4
   character(3) :: c1, c2, c3, c4, c5, c6
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( base :: b1(2), b2(2,2) )


   select type ( b1 )
      type is (base)
         allocate ( b1(1)%u, source = 5 )         !<- integer type
         allocate ( b1(2)%u, source = 'abc' )     !<- character type
   end select

   select type ( b2 )
      type is (base)
         allocate ( b2(1,1)%u, source = datum() ) !<- datum type
         allocate ( b2(2,1)%u, source = 'def' )   !<- character type
         allocate ( b2(1,2)%u, source = 6 )       !<- integer type
         allocate ( b2(2,2)%u, source = 'ghi' )   !<- character type
   end select

   open (unit = 1, file ='array003a.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   select type ( b1 )
      class is (base)
         write (1, iostat=stat, iomsg=msg )             b1((/2,1/))
      select type ( g => b2 )
         class is (base)
            write (1, iostat=stat, iomsg=msg )          g
      end select
   end select

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1, i1
   read (1, iostat=stat, iomsg=msg )              c2, c3, i2, c4

   print *, c1, i1
   print *, c2, c3, i2, c4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   interface write(unformatted)
      subroutine writeUnformatteddatum (dtv, unit, iostat, iomsg)
         import datum
         class(datum), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   associate ( g => dtv%u )
   select type ( g )
      type is (integer)
         write (unit, iostat=iostat, iomsg=iomsg ) g
      type is (character(*))
         write (unit, iostat=iostat, iomsg=iomsg ) g
      type is (datum)
         write (unit, iostat=iostat, iomsg=iomsg ) g
         if ( iomsg /= 'datumwrite' ) error stop 5_4
   end select
   end associate

end subroutine

subroutine writeUnformatteddatum (dtv, unit, iostat, iomsg)
   use m1
   class(datum), intent(in) :: dtv
   integer,  intent(in) :: unit
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   write ( unit, iostat = iostat ) dtv%c
   iomsg = 'datumwrite'
end subroutine
