!Copyright (C) 2007 Ashwith Jerome Rego
!
!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.


PROGRAM EVAL
  IMPLICIT NONE
  REAL, ALLOCATABLE, DIMENSION(:) :: func
  REAL :: x, y, evalfunction
  INTEGER :: i, n
  
  PRINT *, "Enter degree of polynomial"
  READ *, n
  
  ALLOCATE(func(n+1))
  
  PRINT*,"Enter coefficients starting from highest degree term"
  READ*,(func(i), i = 1 ,n+1)
  
  PRINT *,"Enter value of independent variable"
  READ *, x
  
  y = evalfunction(func,n,x)
  
  PRINT*, "Value of polynomial at x = ",x," is ",y
END PROGRAM EVAL
!Function to evaluate a polynomial using Horner's method
!Author : Louisda16th a.k.a Ashwith J. Rego
!Description: Horner's method is pretty straightforward. Suppose you have a function say
!3 x^3 + 2 x^2 + x + 2
!This can be written as
! ( ( 3x+2 ) x + 1 ) x + 2
!This may seem very elementary. But a closer look shows that you'll have to perform fewer multiplications
!This increases the speed of calculation which is important when you are dealing with a higher degree polynomial.
REAL FUNCTION evalfunction(f , n, x)
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: n !Degree of polynomial
  REAL, INTENT(IN), DIMENSION(n+1) :: f !Coefficients of polynomial
  REAL, INTENT(IN) :: x !Value of independent variable
  
  INTEGER :: i
  
  !Evaluate using Horner's Method
  evalfunction = f(1)*x
  DO i = 2, n
    evalfunction = (evalfunction + f(i))*x
  END DO
  evalfunction = evalfunction + f(n+1)
  
END FUNCTION evalfunction
