set cntrparam levels discrete __LEVEL
set table "exptest-contours-__LEVEL"
splot [__X_MIN:__X_MAX][__Y_MIN:__Y_MAX] f(x, y)

