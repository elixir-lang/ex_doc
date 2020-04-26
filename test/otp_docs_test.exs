defmodule OTPDocsTest do
  use ExUnit.Case, async: true

  test "otp" do
    app = :edoc
    Application.ensure_loaded(app)
    vsn = List.to_string(Application.spec(app)[:vsn])

    ExDoc.generate_docs(to_string(app), vsn,
      app: app,
      source_beam: Application.app_dir(app, "ebin"),
      output: "otp_docs/#{app}"
    )
  end
end
