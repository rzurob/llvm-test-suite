! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 15 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_set_underflow_mode(gradual)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*  test if ieee_set_underflow_mode(gradual) conforms with F2003 standard based on argument perspective,pass a user defined function to SET subroutine
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      module m
         logical :: underflowmode,mode1,mode2
         logical,target :: tmode1,tmode2
         logical,pointer :: pmode1,pmode2
         logical,allocatable :: amode1,amode2
         real :: r
         double precision :: d

         contains

            logical function modulePro1(mode)
               logical,intent(in) :: mode
               modulePro1 = mode
            end function

            function modulePro2(pmode)
               logical,pointer  :: pmode
               logical,pointer  :: modulePro2
               modulePro2 => pmode
            end function

            function modulePro3(amode)
               logical,allocatable,intent(in) :: amode
               logical,allocatable       :: modulePro3
               allocate(modulePro3)
               modulePro3 = amode
            end function
      end module

      program underflowMode3
         use,intrinsic :: ieee_arithmetic
         use m
         implicit none

         interface
            logical function externalFun1(mode)
               logical,intent(in) :: mode
            end function

            function externalFun2(tmode)
               logical,target,intent(in) :: tmode
               logical,pointer           :: externalFun2
            end function

            function externalFun3(amode)
               logical,allocatable,intent(in) :: amode
               logical,allocatable            :: externalFun3
            end function
         end interface

         allocate(amode1,amode2)
         mode1  = .true.
         tmode1 = .true.
         amode1 = .true.
         mode2  = .false.
         tmode2 = .false.
         amode2 = .false.
         pmode1 => tmode1
         pmode2 => tmode2

!        pass an internal function to SET subroutine with return type being default logical scalar
         if(ieee_support_underflow_control(r)) then
            call ieee_set_underflow_mode(internalFun1(mode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 101_4
            call ieee_set_underflow_mode(internalFun1(mode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 102_4
            error stop 103_4
         endif

!        pass an internal function to SET subroutine with return type being pointer of default logical type
         if(ieee_support_underflow_control(d)) then
            call ieee_set_underflow_mode(internalFun2(tmode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 104_4
            call ieee_set_underflow_mode(internalFun2(tmode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 105_4
            error stop 106_4
         endif

!        pass an internal function to SET subroutine with return type being allocatable of default logical type
         if(ieee_support_underflow_control()) then
            call ieee_set_underflow_mode(internalFun3(amode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 107_4
            call ieee_set_underflow_mode(internalFun3(amode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 108_4
            error stop 109_4
         endif

!        pass a module procedure to SET subroutine with return type being default logical type
         if(ieee_support_underflow_control(r)) then
            call ieee_set_underflow_mode(modulePro1(mode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 110_4
            call ieee_set_underflow_mode(modulePro1(mode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 111_4
            error stop 112_4
         endif

!        pass a module procedure to SET subroutine with return type being a pointer of default logical type
         if(ieee_support_underflow_control(d)) then
            call ieee_set_underflow_mode(modulePro2(pmode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 113_4
            call ieee_set_underflow_mode(modulePro2(pmode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 114_4
            error stop 115_4
         endif

!        pass a module procedure to SET subroutine with return type being an allocatable of default logical type
         if(ieee_support_underflow_control()) then
            call ieee_set_underflow_mode(modulePro3(amode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 116_4
            call ieee_set_underflow_mode(modulePro3(amode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 117_4
            error stop 118_4
         endif

!        pass an external function to SET subroutine with return type being a default logical type
         if(ieee_support_underflow_control(r)) then
            call ieee_set_underflow_mode(externalFun1(mode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 119_4
            call ieee_set_underflow_mode(externalFun1(mode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 120_4
            error stop 121_4
         endif

!        pass an external function to SET subroutine with return type being a pointer of default logical type
         if(ieee_support_underflow_control(d)) then
            call ieee_set_underflow_mode(externalFun2(tmode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 122_4
            call ieee_set_underflow_mode(externalFun2(tmode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 123_4
            error stop 124_4
         endif

!        pass an external function to SET subroutine with return type being an allocatable of default logical type
         if(ieee_support_underflow_control()) then
            call ieee_set_underflow_mode(externalFun3(amode1))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 125_4
            call ieee_set_underflow_mode(externalFun3(amode2))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 126_4
            error stop 127_4
         endif

         deallocate(amode1,amode2)
         nullify(pmode1,pmode2)

         contains
            logical function internalFun1(mode)
               logical,intent(in) :: mode
               internalFun1 = mode
            end function

            function internalFun2(tmode)
               logical,target,intent(in) :: tmode
               logical,pointer    :: internalFun2
               internalFun2 => tmode
            end function

            function internalFun3(amode)
               logical,allocatable,intent(in) :: amode
               logical,allocatable    :: internalFun3
               allocate(internalFun3)
               internalFun3 = amode
            end function

      end program

            logical function externalFun1(mode)
               logical,intent(in) :: mode
               externalFun1 = mode
            end function

            function externalFun2(tmode)
               logical,target,intent(in) :: tmode
               logical,pointer    :: externalFun2
               externalFun2 => tmode
            end function

            function externalFun3(amode)
               logical,allocatable,intent(in) :: amode
               logical,allocatable    :: externalFun3
               allocate(externalFun3)
               externalFun3 = amode
            end function
