C***********************************************************************
C*  ===================================================================
C*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
C*  ===================================================================
C*
C*  TEST CASE NAME             : FXIF0001
C*  TEST CASE TITLE            : Intrinsic Functions on Derived Type Component
C*
C*  PROGRAMMER                 : Alice Ngai
C*  DATE                       : 10 Feb 1992
C*  ORIGIN                     : AIX Compiler Development, Toronto Lab
C*
C*  PRIMARY FUNCTIONS TESTED   : INT
C*  SECONDARY FUNCTIONS TESTED : None
C*
C*  DESCRIPTION                : INT(x)
C*  KEYWORD(S)                 : INT, COMPONENTS
C*  NUMBER OF TESTS            : 21
C*  STATUS                     : DONE
C*
C*  STRUCTURE                  : Main program
C*  EXECUTABLE                 : Yes
C*
C*  INPUTS                     : None
C*  OUTPUTS                    : None
C*
C*  SETUP REQUIREMENTS         : N/A
C*  DEPENDENCIES               : External routine ZZRC
C*  REQUIRED COMPILER OPTIONS  : None
C*
C*  NORMAL COMPLETION          : Return code = 0
C*  ABNORMAL COMPLETION        : Return code ^= 0
C*
C*  RUN TIME ESTIMATE          : <60 SECS
C*
C*  CONDITIONS TESTED          : Listed below.
C*
C/  COND.   DESCRIPTION
C/  -----   ------------------------------------------------------------
C/          INTRINSIC FUNCTION -- INT on Integer components of derived types
C/  1-7     Derived type components (1-deep)
C/  8-14    Derived type components (2-deep)
C/  15-21   Derived type components (3-deep)
C* ===================================================================
C*
C*  REVISION HISTORY
C*
C*  MM/DD/YY:  Init:  Comments:
C*  02/10/92   AN     -Initial version
C*
C* ===================================================================
C234567890123456789012345678901234567890123456789012345678901234567890


      PROGRAM FXIF0001
         use, intrinsic :: ISO_FORTRAN_ENV
         implicit none
         integer caseid /0/
         logical equal /.false./

C*******************************************************
C* BEGIN:        Symbolic Constants                    *
C*******************************************************

         integer(int8) i_sc
            parameter (i_sc   = 50000)
         integer*2 i2_sc
            parameter (i2_sc  = 20000)
         integer*4 i4_sc
            parameter (i4_sc  = 50000)

C*******************************************************
C* END:          Symbolic Constants                    *
C*******************************************************


C*******************************************************
C*  BEGIN:        Variables  Declaration               *
C*******************************************************

         integer(int8) result_var1
         integer*4 result_var2, result_var3

C*******************************************************
C*  END:          Variables  Declaration               *
C*******************************************************

C*******************************************************
C*  BEGIN:  Initialize expected answers.               *
C*******************************************************
         integer(int8) expected_i
            parameter (expected_i   = 50000)
         integer*4 expected_i2
            parameter (expected_i2  = 20000)
         integer*4 expected_i4
            parameter (expected_i4  = 50000)
C*******************************************************
C*  END:    Initialize expected answers.               *
C*******************************************************:w

C*******************************************************
C*  BEGIN:       Derived Types                         *
C*******************************************************


C -> the "basic" derived types (used in defining all other types)

C A derived type whose components  of type INTEGER.
      type integer_test

         integer(int8) i
         integer*2 i2
         integer*4 i4
         
      end type integer_test
 
C A derived type whose components are one-dimensional arrays of INTEGER type.
      type integer_arr

         integer(int8) i(3)
         integer*2 i2(3)
         integer*4 i4(3)

      end type integer_arr

C A derived type whose components are two-dimensional arrays of INTEGER type.
      type integer_dbl
  
         integer(int8) i(3,3)
         integer*2 i2(3,3)
         integer*4 i4(3,3) 

      end type integer_dbl

C/ ->  Constant derived object which is one level deep
      type (integer_test) child_sc              ! child_sc%x
      type (integer_test) child_var		! child_sc%x
      type (integer_arr) child_arr              ! child_arr%x(1)
      type (integer_dbl) child_dbl              ! child_arr%x(1,1)

      type (integer_test) child_var_array(3,3)  ! child_var_array(1,1)%x
      type (integer_arr) child_arr_array(3,3)   ! child_arr_array(1,1)%x(1)
      type (integer_dbl) child_dbl_array(3,3)   ! child_dbl_array(1,1)%x(1,1)

C/ ->  Constant derived object which is two levels deep
      type two_deep_sc
         type (integer_test) child_sc
      end type two_deep_sc
      type (two_deep_sc) parent_sc              ! parent_sc%child_sc%x

      type two_deep_var
         type (integer_test) child_var
      end type two_deep_var
      type (two_deep_var) parent_var            ! parent_var%child_var%x

      type two_deep_arr 
         type (integer_arr) child_arr
      end type two_deep_arr
      type (two_deep_arr) parent_arr            ! parent_arr%child_arr%x(1)

      type two_deep_dbl
         type (integer_dbl) child_dbl
      end type two_deep_dbl
      type (two_deep_dbl) parent_dbl            ! parent_dbl%child_dbl%x(1,1)

      type two_deep_var_array
         type (integer_test) child_var_array(3,3)
      end type two_deep_var_array
      type (two_deep_var_array) parent_var_array(3,3) 
c/ parent_var_array(1,1)%child_var_array(1,1)%x

      type two_deep_arr_array
         type (integer_arr) child_arr_array(3,3)
      end type two_deep_arr_array
      type (two_deep_arr_array) parent_arr_array(3,3)
c/ parent_arr_array(1,1)%child_arr_array(1,1)%x(1)

      type two_deep_dbl_array
         type (integer_dbl) child_dbl_array(3,3)
      end type two_deep_dbl_array
      type (two_deep_dbl_array) parent_dbl_array(3,3)
c/ parent_dbl_array(1,1)%child_dbl_array(1,1)%x(1,1)
         
C/ ->  Constant derived object which is three levels deep
      type three_deep_sc
         type (two_deep_sc) parent_sc
      end type three_deep_sc
      type (three_deep_sc) grandparent_sc
c/ grandparent_sc%parent_sc%child_sc%x

      type three_deep_var
         type (two_deep_var) parent_var
      end type three_deep_var
      type (three_deep_var) grandparent_var
c/ grandparent_var%parent_var%child_var%x

      type three_deep_arr
         type (two_deep_arr) parent_arr
      end type three_deep_arr
      type (three_deep_arr) grandparent_arr
c/ grandparent_arr%parent_arr%child_arr%x(1)

      type three_deep_dbl
         type (two_deep_dbl) parent_dbl
      end type three_deep_dbl
      type (three_deep_dbl) grandparent_dbl
c/ grandparent_dbl%parent_dbl%child_dbl%x(1,1)

      type three_deep_var_array
         type (two_deep_var_array) parent_var_array(3,3)
      end type three_deep_var_array
      type (three_deep_var_array) grandparent_var_array(3,3)
c/ grandparent_var_array(1,1)%parent_var_array(1,1)%child_var_array(1,1)%x

      type three_deep_arr_array
         type (two_deep_arr_array) parent_arr_array(3,3)
      end type three_deep_arr_array
      type (three_deep_arr_array) grandparent_arr_array(3,3)
c/ grandparent_arr_array(1,1)%parent_arr_array(1,1)%child_arr_array(1,1)%x(1)

      type three_deep_dbl_array
         type (two_deep_dbl_array) parent_dbl_array(3,3)
      end type three_deep_dbl_array
      type (three_deep_dbl_array) grandparent_dbl_array(3,3)
c/ grandparent_dbl_array(1,1)%parent_dbl_array(1,1)%child_dbl_array(1,1)%x(1,1)

C*******************************************************
C*  END:         Derived Types                         *
C*******************************************************

C*******************************************************
C*  BEGIN:   Initialization of Derived Types           *
C*******************************************************

C/ Initialize all symbolic constants
C/ ->  Initialize child_sc
      parameter (child_sc = integer_test (50000,20000,50000))

C/ ->  Initialize parent_sc
      parameter (parent_sc = two_deep_sc (child_sc))

C/ ->  Initialize grandparent_sc
      parameter (grandparent_sc = three_deep_sc (parent_sc))

C/ Initialize all variables
C/ ->  Initialize child_var 
      child_var = integer_test(i_sc, i2_sc, i4_sc)

C/ ->  Initialize parent_var
      parent_var = two_deep_var(child_var)

C/ ->  Initialize grandparent_var
      grandparent_var = three_deep_var(parent_var)

C/ ->  Initialize child_var_array(2,2)
      child_var_array(2,2)%i = i_sc
      child_var_array(2,2)%i2 = i2_sc
      child_var_array(2,2)%i4 = i4_sc

C/ ->  Initialize parent_var_array(2,2)
      parent_var_array(2,2)%child_var_array(2,2)%i = i_sc
      parent_var_array(2,2)%child_var_array(2,2)%i2 = i2_sc
      parent_var_array(2,2)%child_var_array(2,2)%i4 = i4_sc

C/ ->  Initialize grandparent_var_array(2,2)
      grandparent_var_array(2,2)%parent_var_array(2,2)%
     + child_var_array(2,2)%i = i_sc
      grandparent_var_array(2,2)%parent_var_array(2,2)%
     + child_var_array(2,2)%i2 = i2_sc
      grandparent_var_array(2,2)%parent_var_array(2,2)%
     + child_var_array(2,2)%i4 = i4_sc

C/ Initialize single arrays
C/ ->  Initialize child_arr
      child_arr%i(2) = i_sc
      child_arr%i2(2) = i2_sc
      child_arr%i4(2) = i4_sc

C/ ->  Initialize parent_arr
      parent_arr%child_arr%i(2) = i_sc
      parent_arr%child_arr%i2(2) = i2_sc
      parent_arr%child_arr%i4(2) = i4_sc

C/ ->  Initialize grandparent_arr
      grandparent_arr%parent_arr%child_arr%i(2) = i_sc
      grandparent_arr%parent_arr%child_arr%i2(2) = i2_sc
      grandparent_arr%parent_arr%child_arr%i4(2) = i4_sc

C/ ->  Initialize child_arr_array(2,2)
      child_arr_array(2,2)%i(2) = i_sc
      child_arr_array(2,2)%i2(2) = i2_sc
      child_arr_array(2,2)%i4(2) = i4_sc

C/ ->  Initialize parent_arr_array(2,2)
      parent_arr_array(2,2)%child_arr_array(2,2)%i(2) = i_sc
      parent_arr_array(2,2)%child_arr_array(2,2)%i2(2) = i2_sc
      parent_arr_array(2,2)%child_arr_array(2,2)%i4(2) = i4_sc

C/ ->  Initialize grandparent_arr_array(2,2)
      grandparent_arr_array(2,2)%parent_arr_array(2,2)%
     + child_arr_array(2,2)%i(2) = i_sc
      grandparent_arr_array(2,2)%parent_arr_array(2,2)%
     + child_arr_array(2,2)%i2(2) = i2_sc
      grandparent_arr_array(2,2)%parent_arr_array(2,2)%
     + child_arr_array(2,2)%i4(2) = i4_sc

C/ Initialize double arrays
C/ ->  Initialize child_dbl
      child_dbl%i(2,2) = i_sc
      child_dbl%i2(2,2) = i2_sc
      child_dbl%i4(2,2) = i4_sc

C/ ->  Initialize parent_dbl
      parent_dbl%child_dbl%i(2,2) = i_sc
      parent_dbl%child_dbl%i2(2,2) = i2_sc
      parent_dbl%child_dbl%i4(2,2) = i4_sc

C/ ->  Initialize grandparent_dbl
      grandparent_dbl%parent_dbl%child_dbl%i(2,2) = i_sc
      grandparent_dbl%parent_dbl%child_dbl%i2(2,2) = i2_sc
      grandparent_dbl%parent_dbl%child_dbl%i4(2,2) = i4_sc

C/ ->  Initialize child_dbl_array(2,2)
      child_dbl_array(2,2)%i(2,2) = i_sc
      child_dbl_array(2,2)%i2(2,2) = i2_sc
      child_dbl_array(2,2)%i4(2,2) = i4_sc

C/ ->  Initialize parent_dbl_array(2,2)
      parent_dbl_array(2,2)%child_dbl_array(2,2)%i(2,2) = i_sc
      parent_dbl_array(2,2)%child_dbl_array(2,2)%i2(2,2) = i2_sc
      parent_dbl_array(2,2)%child_dbl_array(2,2)%i4(2,2) = i4_sc

C/ ->  Initialize grandparent_dbl_array(2,2)
      grandparent_dbl_array(2,2)%parent_dbl_array(2,2)%
     + child_dbl_array(2,2)%i(2,2) = i_sc
      grandparent_dbl_array(2,2)%parent_dbl_array(2,2)%
     + child_dbl_array(2,2)%i2(2,2) = i2_sc
      grandparent_dbl_array(2,2)%parent_dbl_array(2,2)%
     + child_dbl_array(2,2)%i4(2,2) = i4_sc

C*******************************************************
C*  END:     Initialization of Derived Types           *
C*******************************************************

C*******************************************************
C*  BEGIN:  Test Cases                                 *
C*******************************************************

C/ Test cases for derived type components (1-deep)
      caseid = 1
      result_var1 = int(child_sc%i)
      result_var2 = int(child_sc%i2)
      result_var3 = int(child_sc%i4)
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 2
      result_var1 = int(child_var%i)
      result_var2 = int(child_var%i2)
      result_var3 = int(child_var%i4)
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 3
      result_var1 = int(child_arr%i(2))
      result_var2 = int(child_arr%i2(2))
      result_var3 = int(child_arr%i4(2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)
      
      caseid = 4
      result_var1 = int(child_dbl%i(2,2))
      result_var2 = int(child_dbl%i2(2,2))
      result_var3 = int(child_dbl%i4(2,2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 5
      result_var1 = int(child_var_array(2,2)%i)
      result_var2 = int(child_var_array(2,2)%i2)
      result_var3 = int(child_var_array(2,2)%i4)
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 6
      result_var1 = int(child_arr_array(2,2)%i(2)) 
      result_var2 = int(child_arr_array(2,2)%i2(2))
      result_var3 = int(child_arr_array(2,2)%i4(2)) 
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)
      
      caseid = 7
      result_var1 = int(child_dbl_array(2,2)%i(2,2))
      result_var2 = int(child_dbl_array(2,2)%i2(2,2))
      result_var3 = int(child_dbl_array(2,2)%i4(2,2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

C/ Test cases for derived type components (2-deep)
      caseid = 8
      result_var1 = int(parent_sc%child_sc%i) 
      result_var2 = int(parent_sc%child_sc%i2)
      result_var3 = int(parent_sc%child_sc%i4) 
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 9
      result_var1 = int(parent_var%child_var%i)
      result_var2 = int(parent_var%child_var%i2)
      result_var3 = int(parent_var%child_var%i4) 
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 10
      result_var1 = int(parent_arr%child_arr%i(2))
      result_var2 = int(parent_arr%child_arr%i2(2))
      result_var3 = int(parent_arr%child_arr%i4(2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)
      
      caseid = 11
      result_var1 = int(parent_dbl%child_dbl%i(2,2))
      result_var2 = int(parent_dbl%child_dbl%i2(2,2))
      result_var3 = int(parent_dbl%child_dbl%i4(2,2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 12
      result_var1 = int(parent_var_array(2,2)%child_var_array(2,2)%i) 
      result_var2 = int(parent_var_array(2,2)%child_var_array(2,2)%i2) 
      result_var3 = int(parent_var_array(2,2)%child_var_array(2,2)%i4) 
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 13
      result_var1 = int(parent_arr_array(2,2)%child_arr_array(2,2)%i(2)) 
      result_var2 = int(parent_arr_array(2,2)%child_arr_array(2,2)%
     + i2(2))
      result_var3 = int(parent_arr_array(2,2)% child_arr_array(2,2)%
     + i4(2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)
      
      caseid = 14
      result_var1 = int(parent_dbl_array(2,2)%child_dbl_array(2,2)%
     + i(2,2)) 
      result_var2 = int(parent_dbl_array(2,2)%child_dbl_array(2,2)%
     + i2(2,2))
      result_var3 = int(parent_dbl_array(2,2)%child_dbl_array(2,2)%
     + i4(2,2)) 
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

C/ Test cases for derived type components (3-deep)
      caseid = 15
      result_var1 = int(grandparent_sc%parent_sc%child_sc%i)
      result_var2 = int(grandparent_sc%parent_sc%child_sc%i2)
      result_var3 = int(grandparent_sc%parent_sc%child_sc%i4) 
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 16
      result_var1 = int(grandparent_var%parent_var%child_var%i)
      result_var2 = int(grandparent_var%parent_var%child_var%i2)
      result_var3 = int(grandparent_var%parent_var%child_var%i4)
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 17
      result_var1 = int(grandparent_arr%parent_arr%child_arr%i(2))
      result_var2 = int(grandparent_arr%parent_arr%child_arr%i2(2))
      result_var3 = int(grandparent_arr%parent_arr%child_arr%i4(2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)
      
      caseid = 18
      result_var1 = int(grandparent_dbl%parent_dbl%child_dbl%i(2,2))
      result_var2 = int(grandparent_dbl%parent_dbl%child_dbl%i2(2,2))
      result_var3 = int(grandparent_dbl%parent_dbl%child_dbl%i4(2,2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 19
      result_var1 = int(grandparent_var_array(2,2)%
     + parent_var_array(2,2)%child_var_array(2,2)%i)
      result_var2 = int(grandparent_var_array(2,2)%
     + parent_var_array(2,2)%child_var_array(2,2)%i2)
      result_var3 = int(grandparent_var_array(2,2)%
     + parent_var_array(2,2)%child_var_array(2,2)%i4)
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      caseid = 20
      result_var1 = int(grandparent_arr_array(2,2)%
     + parent_arr_array(2,2)%child_arr_array(2,2)%i(2))
      result_var2 = int(grandparent_arr_array(2,2)%
     + parent_arr_array(2,2)%child_arr_array(2,2)%i2(2))
      result_var3 = int(grandparent_arr_array(2,2)%
     + parent_arr_array(2,2)%child_arr_array(2,2)%i4(2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)
      
      caseid = 21
      result_var1 = int(grandparent_dbl_array(2,2)%
     + parent_dbl_array(2,2)%child_dbl_array(2,2)%i(2,2))
      result_var2 = int(grandparent_dbl_array(2,2)%
     + parent_dbl_array(2,2)%child_dbl_array(2,2)%i2(2,2))
      result_var3 = int(grandparent_dbl_array(2,2)%
     + parent_dbl_array(2,2)%child_dbl_array(2,2)%i4(2,2))
      call check(result_var1, result_var2, result_var3, equal)
      if (.not. equal) call zzrc(caseid)

      contains
         subroutine check(result1,result2,result3,equal)
            integer(int8) result1
            integer*4 result2, result3
            logical equal
            equal = result1 .eq. expected_i .and.
     +              result2 .eq. expected_i2 .and.
     +              result3 .eq. expected_i4  
         end subroutine check
      end
C*******************************************************
C*  END:    Test Cases                                 *
C*******************************************************
