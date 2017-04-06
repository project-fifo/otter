-record(span , {
    timestamp   :: otters:time_us(),           % timestamp of starting the span
    trace_id    :: otters:trace_id(),          % 64 bit integer trace id
    name        :: otters:info(),              % name of the span
    id          :: otters:span_id(),           % 64 bit integer span id
    parent_id   :: otters:span_id() | undefined, % 64 bit integer parent span id
    tags = []   :: [{otters:info(), otters:info()} |
                    {otters:info(), otters:info(), otters:service()}],  % span tags
    logs = []   :: [{otters:time_us(), otters:info()} |
                    {otters:info(), otters:info(), otters:service()}], % span logs
    duration    :: otters:time_us()            % microseconds between span start/end
}).
