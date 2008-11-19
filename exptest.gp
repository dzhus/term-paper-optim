f(x, y) = (exp(-0.1*x)-exp(-0.1*y)-(exp(-0.1)-exp(-1)))**2+\
          (exp(-0.2*x)-exp(-0.2*y)-(exp(-0.2)-exp(-2)))**2+\
          (exp(-0.3*x)-exp(-0.3*y)-(exp(-0.3)-exp(-3)))**2+\
          (exp(-0.4*x)-exp(-0.4*y)-(exp(-0.4)-exp(-4)))**2+\
          (exp(-0.5*x)-exp(-0.5*y)-(exp(-0.5)-exp(-5)))**2+\
          (exp(-0.6*x)-exp(-0.6*y)-(exp(-0.6)-exp(-6)))**2+\
          (exp(-0.7*x)-exp(-0.7*y)-(exp(-0.7)-exp(-7)))**2+\
          (exp(-0.8*x)-exp(-0.8*y)-(exp(-0.8)-exp(-8)))**2+\
          (exp(-0.9*x)-exp(-0.9*y)-(exp(-0.9)-exp(-9)))**2+\
          (exp(-1.0*x)-exp(-1.0*y)-(exp(-1.0)-exp(-10)))**2

set contour
unset surface
set format "%.5f"

set isosamples 100
set cntrparam levels discrete 0.003
set table "exptest-contours-0.003.out"
splot [-1:5][0:12] f(x, y)

set cntrparam levels discrete 0.01
set table "exptest-contours-0.01.out"
splot [-1:5][0:12] f(x, y)

set isosamples 25
set cntrparam levels discrete 0.05
set table "exptest-contours-0.05.out"
splot [-1:5][0:12] f(x, y)

set cntrparam levels discrete 0.1
set table "exptest-contours-0.1.out"
splot [-1:5][0:12] f(x, y)

set cntrparam levels discrete 0.2
set table "exptest-contours-0.2.out"
splot [-1:5][0:12] f(x, y)

set cntrparam levels discrete 0.5
set table "exptest-contours-0.5.out"
splot [-1:5][0:12] f(x, y)

set cntrparam levels discrete 0.8
set table "exptest-contours-0.8.out"
splot [-1:5][0:12] f(x, y)
