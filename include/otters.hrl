-record(span, {
          %% strt timestamp
          timestamp  :: otters:time_us(),
          %% 64 bit int trace id
          trace_id   :: otters:trace_id() | undefined,
          %% name of the span
          name       :: otters:info(),
          %% 64 bit int span id
          id         :: otters:span_id() | undefined,
          %% 64 bit int parent span
          parent_id  :: otters:span_id() | undefined,
          %% Tags
          tags = #{} :: otter:tags(),
          %%  logs
          logs = []  :: [{otters:time_us(), otters:info()} |
                         {otters:info(), otters:info(),
                          otters:service() | undefined}],
          %% microseconds between span start/end
          duration   :: otters:time_us()
         }).
