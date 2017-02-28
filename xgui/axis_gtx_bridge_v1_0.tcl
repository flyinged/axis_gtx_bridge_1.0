# Definitional proc to organize widgets for parameters.
proc init_gui { IPINST } {
  ipgui::add_param $IPINST -name "Component_Name"
  ipgui::add_param $IPINST -name "SIM_RESET_SPEEDUP"
  ipgui::add_param $IPINST -name "CPU_CLK_Hz"
  ipgui::add_param $IPINST -name "REF_CLK_Hz"
  ipgui::add_param $IPINST -name "BAUD_RATE_Mbps" -widget comboBox

}

proc update_PARAM_VALUE.BAUD_RATE_Mbps { PARAM_VALUE.BAUD_RATE_Mbps } {
	# Procedure called to update BAUD_RATE_Mbps when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.BAUD_RATE_Mbps { PARAM_VALUE.BAUD_RATE_Mbps } {
	# Procedure called to validate BAUD_RATE_Mbps
	return true
}

proc update_PARAM_VALUE.CPU_CLK_Hz { PARAM_VALUE.CPU_CLK_Hz } {
	# Procedure called to update CPU_CLK_Hz when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.CPU_CLK_Hz { PARAM_VALUE.CPU_CLK_Hz } {
	# Procedure called to validate CPU_CLK_Hz
	return true
}

proc update_PARAM_VALUE.C_M00_AXIS_TDATA_WIDTH { PARAM_VALUE.C_M00_AXIS_TDATA_WIDTH } {
	# Procedure called to update C_M00_AXIS_TDATA_WIDTH when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.C_M00_AXIS_TDATA_WIDTH { PARAM_VALUE.C_M00_AXIS_TDATA_WIDTH } {
	# Procedure called to validate C_M00_AXIS_TDATA_WIDTH
	return true
}

proc update_PARAM_VALUE.C_S00_AXIS_TDATA_WIDTH { PARAM_VALUE.C_S00_AXIS_TDATA_WIDTH } {
	# Procedure called to update C_S00_AXIS_TDATA_WIDTH when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.C_S00_AXIS_TDATA_WIDTH { PARAM_VALUE.C_S00_AXIS_TDATA_WIDTH } {
	# Procedure called to validate C_S00_AXIS_TDATA_WIDTH
	return true
}

proc update_PARAM_VALUE.REF_CLK_Hz { PARAM_VALUE.REF_CLK_Hz } {
	# Procedure called to update REF_CLK_Hz when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.REF_CLK_Hz { PARAM_VALUE.REF_CLK_Hz } {
	# Procedure called to validate REF_CLK_Hz
	return true
}

proc update_PARAM_VALUE.SIM_RESET_SPEEDUP { PARAM_VALUE.SIM_RESET_SPEEDUP } {
	# Procedure called to update SIM_RESET_SPEEDUP when any of the dependent parameters in the arguments change
}

proc validate_PARAM_VALUE.SIM_RESET_SPEEDUP { PARAM_VALUE.SIM_RESET_SPEEDUP } {
	# Procedure called to validate SIM_RESET_SPEEDUP
	return true
}


proc update_MODELPARAM_VALUE.SIM_RESET_SPEEDUP { MODELPARAM_VALUE.SIM_RESET_SPEEDUP PARAM_VALUE.SIM_RESET_SPEEDUP } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.SIM_RESET_SPEEDUP}] ${MODELPARAM_VALUE.SIM_RESET_SPEEDUP}
}

proc update_MODELPARAM_VALUE.CPU_CLK_Hz { MODELPARAM_VALUE.CPU_CLK_Hz PARAM_VALUE.CPU_CLK_Hz } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.CPU_CLK_Hz}] ${MODELPARAM_VALUE.CPU_CLK_Hz}
}

proc update_MODELPARAM_VALUE.REF_CLK_Hz { MODELPARAM_VALUE.REF_CLK_Hz PARAM_VALUE.REF_CLK_Hz } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.REF_CLK_Hz}] ${MODELPARAM_VALUE.REF_CLK_Hz}
}

proc update_MODELPARAM_VALUE.BAUD_RATE_Mbps { MODELPARAM_VALUE.BAUD_RATE_Mbps PARAM_VALUE.BAUD_RATE_Mbps } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.BAUD_RATE_Mbps}] ${MODELPARAM_VALUE.BAUD_RATE_Mbps}
}

proc update_MODELPARAM_VALUE.C_S00_AXIS_TDATA_WIDTH { MODELPARAM_VALUE.C_S00_AXIS_TDATA_WIDTH PARAM_VALUE.C_S00_AXIS_TDATA_WIDTH } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.C_S00_AXIS_TDATA_WIDTH}] ${MODELPARAM_VALUE.C_S00_AXIS_TDATA_WIDTH}
}

proc update_MODELPARAM_VALUE.C_M00_AXIS_TDATA_WIDTH { MODELPARAM_VALUE.C_M00_AXIS_TDATA_WIDTH PARAM_VALUE.C_M00_AXIS_TDATA_WIDTH } {
	# Procedure called to set VHDL generic/Verilog parameter value(s) based on TCL parameter value
	set_property value [get_property value ${PARAM_VALUE.C_M00_AXIS_TDATA_WIDTH}] ${MODELPARAM_VALUE.C_M00_AXIS_TDATA_WIDTH}
}

