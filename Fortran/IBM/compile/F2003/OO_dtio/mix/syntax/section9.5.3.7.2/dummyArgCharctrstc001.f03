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
!*                                  -  Same Type
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

program dummyArgCharctrstc001
   use m1

   interface read(unformatted)
      ! read1: defines different type
      subroutine read1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1), intent(inout) :: dtv
         character(1), intent(in) :: unit         !<- shall be integer type
         real, intent(out) :: iostat              !<- shall be integer type
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      ! read2: defines non-default kind type parameter
      subroutine read2 (dtv, unit, iotype, v_list, iostat, iomsg)
         import base1
         class(base1), intent(inout) :: dtv
         integer, intent(in) :: unit
         integer, intent(in) :: iotype            !<- shall be character(*) type
         character(1), intent(in) :: v_list(:)    !<- shall be integer(4) type
         integer, intent(out) :: iostat
         real, intent(inout) :: iomsg             !<- shall be character(*) type
      end subroutine
   end interface

end program




