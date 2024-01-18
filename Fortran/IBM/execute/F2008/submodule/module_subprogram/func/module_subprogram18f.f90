!*  ===================================================================
!*
!*  DATE                       : December 20, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  based on enum/func/fxenum0025.f
!*
!*  Use enumerators in a submodule defined in host scope
!*
!*  Secondary test:
!*  - bind(c) in module function
!*  - define multiple submodules of a submodule represented by the
!*    following tree structure with module m, submodules m1 - m6:
!*
!*                   m
!*                 /   \
!*                /     \
!*               /       \
!*              m1       m2
!*            /   \     /  \
!*          m3    m4   m5   m6
!*
!*  Verify that the results match the values of the original test case
!*
!234567890123456789012345678901234567890123456789012345678901234567890

! This is Lewis Carroll's Algorithm
! (From http://www.cs.usyd.edu.au/%7Ekev/pp/TUTORIALS/1b/carroll.html)

module dayofweek

   enum, bind(c)
      enumerator :: SUNDAY
      enumerator :: MONDAY=1, TUESDAY, WEDNESDAY=3, THURSDAY
      enumerator FRIDAY, SATURDAY
      enumerator :: WEEK=7
   end enum

   enum, bind(c)
      enumerator :: JAN=1, FEB, MAR, APRIL, MAY
      enumerator JUNE, JULY, AUG, SEPT, OCT, NOV, DEC
      enumerator :: MONTHS=12
   end enum

interface

   module function isOldStyle(year, month, day)
      integer, intent(in) :: year, month, day
      logical isOldStyle
   end function isOldStyle

   module function isLeapyear(year, month, day)
      integer, intent(in) :: year, month, day
      logical isLeapyear
   end function isLeapyear

   module function century_item(year, month, day)
      integer, intent(in) :: year, month, day
      integer century_item
   end function century_item

   module function year_item(year, month, day)
      integer, intent(in) :: year, month, day
      integer year_item
   end function year_item

   module function month_item(year, month, day)
      integer, intent(in) :: year, month, day
      integer month_item
   end function month_item

   module function day_item(year, month, day)
      integer, intent(in) :: year, month, day
      integer day_item
   end function day_item

end interface

end module dayofweek

submodule (dayofweek) dow_functions1
contains

   module procedure isOldStyle

      logical isOldStyle

      if (year .lt. 1752) then
         isOldStyle = .true.
      else if (year .eq. 1752 .and. month .lt. SEPT) then
         isOldStyle = .true.
      else if (year .eq. 1752 .and. month .eq. SEPT .and. &
               day .le. 2) then
         isOldStyle = .true.

      else if (year .eq. 1752 .and. month .eq. SEPT .and. &
               day .lt. 14) then
         print *, 'Illegal date!'
         error stop 66
      else
         isOldStyle = .false.
      end if

   end

end submodule dow_functions1

submodule (dayofweek) dow_functions2
contains

   module procedure isLeapyear

      integer, intent(in) :: year, month, day
      logical isLeapyear

      logical isNewStyle
      isNewStyle= .not. isOldStyle(year, month, day)

      if (year .le. 0) then
         print *, 'Illegal year!'
         error stop 67
      else if (mod(year, 400) .eq. 0) then
         isLeapyear = .true.
      else if (isNewStyle .and. mod(year, 100) .eq. 0) then
         isLeapyear = .false.
      else if (mod(year, 4) .eq. 0) then
         isLeapyear = .true.
      else
         isLeapyear = .false.
      end if

   end

end submodule dow_functions2

submodule (dayofweek) dow_functions3
contains

   module procedure century_item

      integer century_item, century

      century = year/100
      if (isOldStyle(year, month, day)) then
         century_item=18-century
      else
         century_item=2*(3-mod(century, 4))
      end if

   end

end submodule dow_functions3

submodule (dayofweek) dow_functions4
contains

   module procedure year_item

      integer year_item, year1
      integer dozen_no, overplus, four_no

      year1 = mod(year, 100)
      dozen_no = year1/MONTHS
      overplus = mod(year1, MONTHS)
      four_no = overplus/4

      year_item = dozen_no + overplus + four_no
      year_item = mod(year_item, WEEK)

      year_item = year_item + century_item(year, month, day)
      year_item = mod(year_item, WEEK)

   end

end submodule dow_functions4

submodule (dayofweek:dow_functions4) dow_functions5
contains

   module procedure month_item

      integer month_item

      month_item = month_item1(month)+year_item(year, month, day)
      month_item = mod(month_item, WEEK)

   end

   ! helper method, local to this submodule
   recursive function month_item1(month)

      integer, intent(in) :: month
      integer month_item1

      select case(month)
         case (JAN)
            month_item1=0
         case (FEB:MAR)
            month_item1=3
         case (APRIL, JUNE, AUG, OCT)
            month_item1=10-month
         case (MAY, JULY)
            month_item1=month_item1(month-1)+30
         case (SEPT, NOV)
            month_item1=month_item1(month-1)+31
         case (DEC)
            month_item1=12
         case default
            print *, "Illegal month!"
            error stop 68
      end select

      month_item1=mod(month_item1, WEEK)

   end function month_item1

end submodule dow_functions5

submodule (dayofweek:dow_functions4) dow_functions6
contains

   module procedure day_item

      integer day_item

      day_item=day+month_item(year, month, day)
      day_item=mod(day_item, WEEK)

      if (isLeapyear(year, month, day)) then
         if (month .eq. 1 .or. month .eq. 2) then
            day_item=day_item-1
            day_item=mod(day_item, WEEK)
         end if
      end if

   end

end submodule dow_functions6

program  fxenum0025

use dayofweek

implicit none

      if (day_item(2001, JAN, 1) .ne. MONDAY) error stop 1

      if (day_item(1676, FEB, 23) .ne. WEDNESDAY) error stop 2

      if (day_item(1971, MAR, 5) .ne. FRIDAY) error stop 3

      if (day_item(2000, APRIL, 24) .ne. MONDAY) error stop 4

      if (day_item(1864, MAY, 31) .ne. TUESDAY) error stop 5

      if (day_item(1989, JUNE, 4) .ne. SUNDAY) error stop 6

      if (day_item(1867, JULY, 1) .ne. MONDAY) error stop 7

      if (day_item(1778, AUG, 15) .ne. SATURDAY) error stop 8

      if (day_item(1783, SEPT, 18) .ne. THURSDAY) error stop 9

      if (day_item(1991, OCT, 1) .ne. TUESDAY) error stop 10

      if (day_item(2002, NOV, 10) .ne. SUNDAY) error stop 11

      if (day_item(2005, DEC, 31) .ne. SATURDAY) error stop 12

end program fxenum0025

