!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-05-27
!*
!*  PRIMARY FUNCTIONS TESTED   : FINDLOC intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FINDLOC (ARRAY, VALUE, DIM [, MASK, KIND, BACK])
!*                               FINDLOC (ARRAY, VALUE [, MASK, KIND, BACK])
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  program findloc_DERIVETYPE

      type typeA
         integer, allocatable, dimension(:) :: m
         real, allocatable, dimension(:) :: r
         character(3), allocatable, dimension(:) :: ch
         complex, allocatable, dimension(:) :: co
         logical, allocatable, dimension(:) :: l
         byte, allocatable, dimension(:) :: b
      end type typeA

      type typeB
         type (typeA), allocatable:: BTestType
      end type typeB

      type (typeB), dimension(:), allocatable :: typeC_Array

      integer loop_i, loop_j
      integer, dimension(1):: result1, result2, result3, result4
      character(3) :: val_char = "mmm"
      byte :: val_byte = 50

      allocate(typeC_Array(1:6))

     do loop_i=1,size(typeC_Array)
       allocate(typeC_Array(loop_i)%BTestType)

       allocate(typeC_Array(loop_i)%BTestType%m(1:10))
       FORALL (loop_j = 1 : 10)  typeC_Array(loop_i)%BTestType%m(loop_j) = 24

       allocate(typeC_Array(loop_i)%BTestType%r(1:10))
       FORALL (loop_j = 1 : 10)  typeC_Array(loop_i)%BTestType%r(loop_j) = 24.0

       allocate(typeC_Array(loop_i)%BTestType%ch(1:10))
       FORALL (loop_j = 1 : 10)  typeC_Array(loop_i)%BTestType%ch(loop_j) = "abc"

       allocate(typeC_Array(loop_i)%BTestType%co(1:10))
       FORALL (loop_j = 1:10) typeC_Array(loop_i)%BTestType%co(loop_j) = (24,0)


       allocate(typeC_Array(loop_i)%BTestType%l(1:10))
       FORALL (loop_j = 1:10) typeC_Array(loop_i)%BTestType%l(loop_j) = .TRUE.

       allocate(typeC_Array(loop_i)%BTestType%b(1:10))
       FORALL (loop_j = 1:10) typeC_Array(loop_i)%BTestType%b(loop_j) = 24
     end do


      !-- for integer type element --!
      typeC_Array(1)%BTestType%m(1) = 50
      typeC_Array(1)%BTestType%m(5) = 50

      typeC_Array(4)%BTestType%m(3) = 50
      typeC_Array(4)%BTestType%m(6) = 50
      typeC_Array(4)%BTestType%m(8) = 50

      result1 = FINDLOC(typeC_Array(1)%BTestType%m, 50)
      result2 = FINDLOC(typeC_Array(1)%BTestType%m, 50, BACK=.TRUE.)

      result3 = FINDLOC(typeC_Array(4)%BTestType%m, 50)
      result4 = FINDLOC(typeC_Array(4)%BTestType%m, 50, BACK=.TRUE.)

      print *, result1

      if (result1(1) .NE. 1)  ERROR STOP 10
      if (result2(1) .NE. 5)  ERROR STOP 20
      if (result3(1) .NE. 3)  ERROR STOP 30
      if (result4(1) .NE. 8)  ERROR STOP 40


      !-- for real type element --!
      typeC_Array(1)%BTestType%r(1) = 50.0
      typeC_Array(1)%BTestType%r(5) = 50.0

      typeC_Array(4)%BTestType%r(3) = 50.0
      typeC_Array(4)%BTestType%r(6) = 50.0
      typeC_Array(4)%BTestType%r(8) = 50.0

      result1 = FINDLOC(typeC_Array(1)%BTestType%r, 50.0)
      result2 = FINDLOC(typeC_Array(1)%BTestType%r, 50.0, BACK=.TRUE.)

      result3 = FINDLOC(typeC_Array(4)%BTestType%r, 50.0)
      result4 = FINDLOC(typeC_Array(4)%BTestType%r, 50.0, BACK=.TRUE.)

      if (result1(1) .NE. 1)  ERROR STOP 11
      if (result2(1) .NE. 5)  ERROR STOP 21
      if (result3(1) .NE. 3)  ERROR STOP 31
      if (result4(1) .NE. 8)  ERROR STOP 41

      print *, result2

      !-- for character type element --!
      typeC_Array(1)%BTestType%ch(1) = "mmm"
      typeC_Array(1)%BTestType%ch(5) = "mmm"

      typeC_Array(4)%BTestType%ch(3) = "mmm"
      typeC_Array(4)%BTestType%ch(6) = "mmm"
      typeC_Array(4)%BTestType%ch(8) = "mmm"

      result1 = FINDLOC(typeC_Array(1)%BTestType%ch, val_char)
      result2 = FINDLOC(typeC_Array(1)%BTestType%ch, val_char, BACK=.TRUE.)

      result3 = FINDLOC(typeC_Array(4)%BTestType%ch, val_char)
      result4 = FINDLOC(typeC_Array(4)%BTestType%ch, val_char, BACK=.TRUE.)

      if (result1(1) .NE. 1)  ERROR STOP 11
      if (result2(1) .NE. 5)  ERROR STOP 21
      if (result3(1) .NE. 3)  ERROR STOP 31
      if (result4(1) .NE. 8)  ERROR STOP 41

      print *, result1

     !-- for complex type element --!
     typeC_Array(1)%BTestType%co(5) = (50,0)
     result1 = FINDLOC(typeC_Array(1)%BTestType%co, (50,0))

     if (result1(1) .NE. 5) ERROR STOP 40
     print *, result1

     !-- for logical type --!
     typeC_Array(1)%BTestType%l(10) = .FALSE.
     result1 = FINDLOC(typeC_Array(1)%BTestType%l, .FALSE.)

     if (result1(1) .NE. 10) ERROR STOP 50
     print *, result1

     !-- for byte type element --!
     typeC_Array(1)%BTestType%b(1) = 50
     result1 = FINDLOC(typeC_Array(1)%BTestType%b, val_byte)

     if (result1(1) .NE. 1) ERROR STOP 60
     print *,result1

  end




