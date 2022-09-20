# apply the value labels -------------------------------------------
main <- atRfunctions::labeler(data = main,
                              tool = smart_tool,
                              survey_label = "label::English",
                              choice_lable = "label::English",
                              multi_response_sep = " "
)
hh_roster <- atRfunctions::labeler(data = hh_roster,
                                   tool = smart_tool,
                                   survey_label = "label::English",
                                   choice_lable = "label::English",
                                   multi_response_sep = " "
)
child <- atRfunctions::labeler(data = child,
                               tool = smart_tool,
                               survey_label = "label::English",
                               choice_lable = "label::English",
                               multi_response_sep = " "
)
preg_lact_wom <- atRfunctions::labeler(data = preg_lact_wom,
                                       tool = smart_tool,
                                       survey_label = "label::English",
                                       choice_lable = "label::English",
                                       multi_response_sep = " "
)
left <- atRfunctions::labeler(data = left,
                              tool = smart_tool,
                              survey_label = "label::English",
                              choice_lable = "label::English",
                              multi_response_sep = " "
)
died <- atRfunctions::labeler(data = died,
                              tool = smart_tool,
                              survey_label = "label::English",
                              choice_lable = "label::English",
                              multi_response_sep = " "
)
