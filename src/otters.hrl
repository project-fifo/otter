-record(span , {
    timestamp   :: otters:time_us(),              % strt timestamp
    trace_id    :: otters:trace_id() | undefined, % 64 bit int trace id
    name        :: otters:info(),                 % name of the span
    id          :: otters:span_id() | undefined,  % 64 bit int span id
    parent_id   :: otters:span_id() | undefined,  % 64 bit int parent span id
    tags = []   :: [{otters:info(), otters:info()} |
                    {otters:info(), otters:info(), otters:service()}],  % tags
    logs = []   :: [{otters:time_us(), otters:info()} |
                    {otters:info(), otters:info(), otters:service()}], %  logs
    duration    :: otters:time_us()       % microseconds between span start/end
}).
