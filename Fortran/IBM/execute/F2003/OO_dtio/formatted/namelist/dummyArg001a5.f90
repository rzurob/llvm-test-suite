!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummyArg001a5.f
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
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting for derived type object which is a assumed-shape array dummy argument
!*                                        which module procedure invokes inner function
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

   type :: base
      character(3) ::  c
   end type

   type, extends(base) :: child
      integer(4)   ::  i
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   integer :: unit = 1
   class(base), pointer :: b2(:,:)

contains

   subroutine writeBase(dtv,lb1, lb2)
      class(base), intent(in) :: dtv(5:,5:)
      integer, intent(in) :: lb1, lb2

      integer :: stat
      character(200) :: msg

      if (( innerWriteBase(dtv,lb1, lb2) /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

      contains

         integer function innerWriteBase(dtv,lb1, lb2)
            class(base), intent(in) :: dtv(lb1:, lb2:)
            integer, intent(in) :: lb1, lb2

            namelist /nml/ dtv

            write ( unit, nml, iostat=innerWriteBase, iomsg = msg)

         end function

   end subroutine

end module

program dummyArg001a5
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1(:,:)
   type(child)              :: b3(2,2)
   type(child), pointer     :: b4(:,:)

   open (unit, file = 'dummyArg001a5.1', form='formatted', access='stream' )

   allocate(b1(2,2), source = reshape ( source = (/ child(c='abc',i=11), child(c='def',i=12), child(c='ghi',i=13), child(c='jkl',i=14)  /), shape = (/2,2/) ) )
   allocate(b2(2,2), source = reshape ( source = (/ child(c='ABC',i=21), child(c='DEF',i=22), child(c='GHI',i=23), child(c='JKL',i=24)  /), shape = (/2,2/) ) )
   b3 = reshape ( source =  (/ child(c='mno',i=31), child(c='pqr',i=32), child(c='stu',i=33), child(c='vwx',i=34)  /), shape = (/2,2/) )
   allocate(b4(2,2), source = reshape ( source = (/ child(c='MNO',i=41), child(c='PQR',i=42), child(c='STU',i=43), child(c='VWX',i=44)  /), shape = (/2,2/) ) )

   call writeBase(b1,10, 100)
   call writeBase(b2,11, 101)
   call writeBase(b3,12, 102)
   call writeBase(b4,13, 103)

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (base)
         write (unit, "('c= ',A3,1X)", iostat=iostat )        dtv%c
      type is (child)
         write (unit, "('i= ',I4, 1X,'c= ',A3,1X)", iostat=iostat )        dtv%i, dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine

