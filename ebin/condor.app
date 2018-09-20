%% -*- mode: erlang -*-

{application, condor,
 [{description, ""},
  {vsn, "0.3.0"},
  {licenses, ["Apache 2.0"]},
  {applications,
   [kernel,
    stdlib
   ]},
  {modules, [
             condor,
             condor_app,
             condor_sup,
             condor_packet,
             condor_listener,
             condor_listener_sup
            ]},
  {mod, {condor_app, []}},
  {maintainers, []},
  {registered, []},
  {links, []},
  {env,[]}
 ]}.
