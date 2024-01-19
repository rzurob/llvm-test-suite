! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               1) Dummy Argument Characteristics
!*                                  -  Same type parameter
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

   type base1
      integer(4) :: i
   end type

end module

program dummyArgCharctrstc002
   use m1

   interface read(unformatted)
      ! read1: defines different length type parameter
      subroutine read1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(100), intent(inout) :: iomsg   !<- shall be assumed type parameter
      end subroutine
   end interface

   interface write(unformatted)
      ! write1: defines different length and kind type parameters
      subroutine write1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1), intent(in) :: dtv
         integer, intent(in) :: unit
         integer(2_4), intent(out) :: iostat      !<- shall be integer(4_4) type
         character(0), intent(inout) :: iomsg     !<- shall be assumed type parameter
      end subroutine
   end interface

end program

subroutine read1 (dtv, unit, iostat, iomsg)
   use m1
   class(base1), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(100), intent(inout) :: iomsg

   integer(4) temp

   read(unit, iostat=iostat, iomsg=iomsg) temp
   dtv%i = temp
end subroutine


subroutine write1 (dtv, unit, iostat, iomsg)
   use m1
   class(base1), intent(in) :: dtv
   integer, intent(in) :: unit
   integer(2_4), intent(out) :: iostat
   character(0), intent(inout) :: iomsg

   write (unit, iostat=iostat, iomsg=iomsg) dtv%i
end subroutine
