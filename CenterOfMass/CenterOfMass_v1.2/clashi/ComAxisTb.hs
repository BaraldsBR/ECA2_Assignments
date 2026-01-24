module ComAxisTb where
import Clash.Prelude
import Axi
import Common
import qualified Data.List as L

-----------------------------------------------------------------------------------------
-- simPrintState function
-----------------------------------------------------------------------------------------

simPrintState f s [] = []
simPrintState f s (i:is) = h L.++ simPrintState f s' is where
  (s', o) = f s i
  h = "\n-------------"
      L.++ "\ni: " L.++ (show i)
      L.++ "\ns:  " L.++ (show s)
      L.++ "\ns': " L.++ (show s')
      L.++ "\no: " L.++ (show o)
      -- L.++ "\n"


mAxisComSerInp :: [(Maybe (Axi4Stream (Signed 32) (BitVector 4)), Bool)]
mAxisComSerInp =
  [ (Nothing, True)    -- Reset
  , (Nothing, True)
  , (Nothing, False)
  -- Image block 8
  , (Just (Axi4Stream (114) False 0b1111), True)
  , (Just (Axi4Stream (115) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Nothing, True)
  , (Nothing, False)
  , (Nothing, True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (115) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Nothing, True)
  , (Just (Axi4Stream (122) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (122) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Just (Axi4Stream (122) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (124) False 0b1111), True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (126) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (124) False 0b1111), True)
  , (Nothing, True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (126) False 0b1111), True)
  , (Just (Axi4Stream (127) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (127) False 0b1111), True)
  , (Just (Axi4Stream (127) False 0b1111), True)
  , (Just (Axi4Stream (128) False 0b1111), True)
  , (Just (Axi4Stream (128) False 0b1111), True)
  , (Just (Axi4Stream (124) False 0b1111), True)
  , (Just (Axi4Stream (126) False 0b1111), True)
  , (Just (Axi4Stream (126) False 0b1111), True)
  , (Just (Axi4Stream (128) False 0b1111), True)
  , (Just (Axi4Stream (127) False 0b1111), True)
  , (Just (Axi4Stream (128) False 0b1111), True)
  , (Just (Axi4Stream (130) False 0b1111), True)
  , (Just (Axi4Stream (129) True  0b1111), True)
  , (Nothing, False)
  , (Nothing, False)
  , (Nothing, True)
  , (Nothing, False)
  , (Nothing, False)
  -- Image block 9
  , (Just (Axi4Stream (126) False 0b1111), False)
  , (Just (Axi4Stream (126) False 0b1111), False)
  , (Just (Axi4Stream (127) False 0b1111), False)
  , (Just (Axi4Stream (130) False 0b1111), False)
  , (Just (Axi4Stream (132) False 0b1111), False)
  , (Just (Axi4Stream (131) False 0b1111), False)
  , (Just (Axi4Stream (132) False 0b1111), False)
  , (Just (Axi4Stream (133) False 0b1111), False)
  , (Just (Axi4Stream (126) False 0b1111), False)
  , (Just (Axi4Stream (126) False 0b1111), False)
  , (Just (Axi4Stream (129) False 0b1111), False)
  , (Just (Axi4Stream (130) False 0b1111), False)
  , (Just (Axi4Stream (131) False 0b1111), False)
  , (Just (Axi4Stream (131) False 0b1111), False)
  , (Just (Axi4Stream (134) False 0b1111), False)
  , (Just (Axi4Stream (134) False 0b1111), False)
  , (Just (Axi4Stream (126) False 0b1111), False)
  , (Just (Axi4Stream (127) False 0b1111), False)
  , (Just (Axi4Stream (129) False 0b1111), False)
  , (Just (Axi4Stream (130) False 0b1111), False)
  , (Just (Axi4Stream (130) False 0b1111), False)
  , (Just (Axi4Stream (132) False 0b1111), False)
  , (Just (Axi4Stream (134) False 0b1111), False)
  , (Just (Axi4Stream (135) False 0b1111), True)
  , (Just (Axi4Stream (129) False 0b1111), True)
  , (Just (Axi4Stream (130) False 0b1111), True)
  , (Just (Axi4Stream (131) False 0b1111), True)
  , (Just (Axi4Stream (133) False 0b1111), True)
  , (Just (Axi4Stream (134) False 0b1111), True)
  , (Just (Axi4Stream (134) False 0b1111), True)
  , (Just (Axi4Stream (135) False 0b1111), True)
  , (Just (Axi4Stream (135) False 0b1111), True)
  , (Just (Axi4Stream (130) False 0b1111), True)
  , (Just (Axi4Stream (131) False 0b1111), True)
  , (Just (Axi4Stream (132) False 0b1111), True)
  , (Just (Axi4Stream (133) False 0b1111), True)
  , (Just (Axi4Stream (134) False 0b1111), True)
  , (Just (Axi4Stream (135) False 0b1111), True)
  , (Just (Axi4Stream (135) False 0b1111), True)
  , (Just (Axi4Stream (136) False 0b1111), True)
  , (Just (Axi4Stream (131) False 0b1111), True)
  , (Just (Axi4Stream (133) False 0b1111), True)
  , (Just (Axi4Stream (136) False 0b1111), True)
  , (Just (Axi4Stream (137) False 0b1111), True)
  , (Just (Axi4Stream (136) False 0b1111), True)
  , (Just (Axi4Stream (135) False 0b1111), True)
  , (Just (Axi4Stream (139) False 0b1111), True)
  , (Just (Axi4Stream (138) False 0b1111), True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (113) False 0b1111), True)
  , (Just (Axi4Stream (103) False 0b1111), True)
  , (Just (Axi4Stream (106) False 0b1111), True)
  , (Just (Axi4Stream (112) False 0b1111), True)
  , (Just (Axi4Stream (108) False 0b1111), True)
  , (Just (Axi4Stream (136) False 0b1111), True)
  , (Just (Axi4Stream (104) False 0b1111), True)
  , (Just (Axi4Stream (78) False 0b1111), True)
  , (Just (Axi4Stream (66) False 0b1111), True)
  , (Just (Axi4Stream (65) False 0b1111), True)
  , (Just (Axi4Stream (78) False 0b1111), True)
  , (Just (Axi4Stream (100) False 0b1111), True)
  , (Just (Axi4Stream (84) False 0b1111), True)
  , (Just (Axi4Stream (85) True 0b1111), False) -- external slave is not ready
  -- Image block 16
  , (Just (Axi4Stream (24) False 0b1111), False) -- slave is not ready, retransmit
  , (Just (Axi4Stream (24) False 0b1111), False) -- slave is not ready, retransmit
  , (Just (Axi4Stream (24) False 0b1111), True) -- slave is not ready, retransmit
  , (Just (Axi4Stream (32) False 0b1111), True)
  , (Just (Axi4Stream (26) False 0b1111), True)
  , (Just (Axi4Stream (12) False 0b1111), True)
  , (Just (Axi4Stream (38) False 0b1111), True)
  , (Just (Axi4Stream (13) False 0b1111), True)
  , (Just (Axi4Stream (11) False 0b1111), True)
  , (Just (Axi4Stream (11) False 0b1111), True)
  , (Nothing, True)
  , (Nothing, True)
  , (Nothing, True)
  , (Nothing, True)
  , (Nothing, True)
  , (Just (Axi4Stream (48) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (41) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (71) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (15) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (19) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (28) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (23) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (12) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (77) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (66) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (71) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (20) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (37) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (21) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (21) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (13) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (104) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (81) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (63) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (8) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (61) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (55) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (19) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (17) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (129) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (91) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (93) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (17) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (84) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (95) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (19) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (11) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (80) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (39) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (62) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (41) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (73) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (47) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (20) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (11) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (60) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (30) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (20) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (65) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (64) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (19) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (11) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (11) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (32) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (20) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (8) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (44) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (73) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (12) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (10) False 0b1111), False) -- external slave is not ready
  , (Just (Axi4Stream (10) True 0b1111), True) -- Now ready
  -- Image 188
  , (Just (Axi4Stream (144) False 0b1111), True)
  , (Just (Axi4Stream (144) False 0b1111), False)
  , (Just (Axi4Stream (144) False 0b1111), False)
  , (Just (Axi4Stream (144) False 0b1111), False)
  , (Just (Axi4Stream (145) False 0b1111), False)
  , (Just (Axi4Stream (146) False 0b1111), False)
  , (Just (Axi4Stream (146) False 0b1111), False)
  , (Just (Axi4Stream (145) False 0b1111), False)
  , (Just (Axi4Stream (143) False 0b1111), False)
  , (Just (Axi4Stream (143) False 0b1111), True)
  , (Just (Axi4Stream (142) False 0b1111), True)
  , (Just (Axi4Stream (142) False 0b1111), True)
  , (Just (Axi4Stream (143) False 0b1111), True)
  , (Just (Axi4Stream (143) False 0b1111), True)
  , (Just (Axi4Stream (144) False 0b1111), True)
  , (Just (Axi4Stream (143) False 0b1111), True)
  , (Just (Axi4Stream (125) False 0b1111), True)
  , (Just (Axi4Stream (131) False 0b1111), True)
  , (Just (Axi4Stream (134) False 0b1111), True)
  , (Just (Axi4Stream (135) False 0b1111), True)
  , (Just (Axi4Stream (137) False 0b1111), True)
  , (Just (Axi4Stream (139) False 0b1111), True)
  , (Just (Axi4Stream (140) False 0b1111), True)
  , (Just (Axi4Stream (141) False 0b1111), True)
  , (Just (Axi4Stream (101) False 0b1111), True)
  , (Just (Axi4Stream (107) False 0b1111), True)
  , (Just (Axi4Stream (114) False 0b1111), True)
  , (Just (Axi4Stream (112) False 0b1111), True)
  , (Just (Axi4Stream (116) False 0b1111), True)
  , (Just (Axi4Stream (115) False 0b1111), True)
  , (Just (Axi4Stream (115) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Just (Axi4Stream (108) False 0b1111), True)
  , (Just (Axi4Stream (111) False 0b1111), True)
  , (Just (Axi4Stream (116) False 0b1111), True)
  , (Just (Axi4Stream (116) False 0b1111), True)
  , (Just (Axi4Stream (118) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (116) False 0b1111), True)
  , (Just (Axi4Stream (115) False 0b1111), True)
  , (Just (Axi4Stream (104) False 0b1111), True)
  , (Just (Axi4Stream (105) False 0b1111), True)
  , (Just (Axi4Stream (110) False 0b1111), True)
  , (Just (Axi4Stream (114) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (113) False 0b1111), True)
  , (Just (Axi4Stream (114) False 0b1111), True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (123) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Just (Axi4Stream (121) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (116) False 0b1111), True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (119) False 0b1111), True)
  , (Just (Axi4Stream (120) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (117) False 0b1111), True)
  , (Just (Axi4Stream (117) True 0b1111), True)
  ]

mAxisComParInp :: [(Maybe (Axi4Stream (Vec 16 (Unsigned 8)) (BitVector 16)), Bool)]
mAxisComParInp =
  [ (Nothing, True)    -- Reset
  , (Nothing, True)
  , (Nothing, False)
  -- Image block 8
  , (Just (Axi4Stream (114:>115:>117:>118:>119:>120:>120:>120:>115:>117:>117:>119:>120:>121:>121:>122:>Nil) False 0b1111111111111111), True)
  , (Just (Axi4Stream (117:>118:>118:>119:>120:>122:>123:>123:>118:>118:>119:>121:>122:>123:>124:>125:>Nil) False 0b1111111111111111), True)
  , (Just (Axi4Stream (118:>120:>121:>123:>123:>123:>125:>126:>120:>121:>123:>124:>125:>125:>126:>127:>Nil) False 0b1111111111111111), True)
  , (Just (Axi4Stream (121:>123:>125:>125:>127:>127:>128:>128:>124:>126:>126:>128:>127:>128:>130:>129:>Nil) True  0b1111111111111111), True)
  , (Nothing, True)
  , (Nothing, False)
  , (Nothing, True)
  -- Image block 9
  , (Just (Axi4Stream (126:>126:>127:>130:>132:>131:>132:>133:>126:>126:>129:>130:>131:>131:>134:>134:>Nil) False 0b1111111111111111), True)
  , (Just (Axi4Stream (126:>127:>129:>130:>130:>132:>134:>135:>129:>130:>131:>133:>134:>134:>135:>135:>Nil) False 0b1111111111111111), True)
  , (Nothing, False)
  , (Just (Axi4Stream (130:>131:>132:>133:>134:>135:>135:>136:>131:>133:>136:>137:>136:>135:>139:>138:>Nil) False 0b1111111111111111), True)
  , (Nothing, True)
  , (Just (Axi4Stream (125:>120:>113:>103:>106:>112:>108:>136:>104:>78:>66:>65:>78:>100:>84:>85:>Nil)       True  0b1111111111111111), False)
  , (Nothing, False)
  , (Nothing, False)
  , (Nothing, True)
  , (Nothing, True)
  -- Image block 16
  , (Just (Axi4Stream (24:>32:>26:>12:>38:>13:>11:>11:>48:>41:>71:>15:>19:>28:>23:>12:>Nil)  False 0b1111111111111111), False)
  , (Just (Axi4Stream (77:>66:>71:>20:>37:>21:>21:>13:>104:>81:>63:>8:>61:>55:>19:>17:>Nil)  False 0b1111111111111111), False)
  , (Just (Axi4Stream (129:>91:>93:>17:>84:>95:>19:>11:>80:>39:>62:>41:>73:>47:>20:>11:>Nil) False 0b1111111111111111), False)
  , (Just (Axi4Stream (60:>30:>20:>65:>64:>19:>11:>11:>32:>20:>8:>44:>73:>12:>10:>10:>Nil)   True 0b1111111111111111), True)
  , (Nothing, True)
  -- Image block 188
  , (Just (Axi4Stream (144:>144:>144:>144:>145:>146:>146:>145:>143:>143:>142:>142:>143:>143:>144:>143:>Nil) False 0b1111111111111111), False)
  , (Nothing, False)
  , (Just (Axi4Stream (125:>131:>134:>135:>137:>139:>140:>141:>101:>107:>114:>112:>116:>115:>115:>118:>Nil) False 0b1111111111111111), True)
  , (Nothing, False)
  , (Just (Axi4Stream (108:>111:>116:>116:>118:>117:>116:>115:>104:>105:>110:>114:>117:>120:>120:>120:>Nil) False 0b1111111111111111), True)
  , (Nothing, False)
  , (Just (Axi4Stream (113:>114:>119:>120:>119:>123:>121:>121:>117:>116:>119:>119:>120:>117:>117:>117:>Nil) True  0b1111111111111111), True)
  , (Nothing, True)
  ]

