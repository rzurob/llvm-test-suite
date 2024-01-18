! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Character output with namelist formatting on internal files
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

   type base
      character(15) :: c1
   end type

end module

module m1
   use m
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

end module

program character001
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable  :: b1
   class(base), pointer      :: b2

   character(10) :: internalFile(20)
   namelist /NmL1/ b1, b2

   allocate( b1, source = base('abcdEfghiJklmnO'))
   allocate( b2, source = base('klmnOpqrsTuvwxY'))

   write (internalFile,NML=nml1, iostat=stat, iomsg=msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

   ! check if internal files contains correct values

   if ( internalFile(1)  /= " &NML1    " ) error stop 2_4
   if ( internalFile(2)  /= " B1= &D   " ) error stop 3_4
   if ( internalFile(3)  /= " DUM=abcdE" ) error stop 4_4
   if ( internalFile(4)  /= " fghiJklmn" ) error stop 5_4
   if ( internalFile(5)  /= " O        " ) error stop 6_4
   if ( internalFile(6)  /= " /, B2= &D" ) error stop 7_4
   if ( internalFile(7)  /= " DUM=klmnO" ) error stop 8_4
   if ( internalFile(8)  /= " pqrsTuvwx" ) error stop 9_4
   if ( internalFile(9)  /= " Y        " ) error stop 10_4
   if ( internalFile(10) /= " /        " ) error stop 11_4
   if ( internalFile(11) /= " /        " ) error stop 12_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(base) :: dum

   namelist /d/  dum

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   dum%c1 = dtv%c1
   write ( unit, d, iostat = iostat )

   iomsg = 'dtiowrite'

end subroutine
