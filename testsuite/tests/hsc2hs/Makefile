TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

HSC2HS_OPTS = --cc="$(TEST_CC)" $(addprefix --cflag=,$(TEST_CC_OPTS)) --ld=$(TEST_CC) $(addprefix --lflag=,$(TEST_CC_OPTS))

.PHONY: hsc2hs001
hsc2hs001:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: hsc2hs002
hsc2hs002:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: hsc2hs003
hsc2hs003:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: hsc2hs004
hsc2hs004:
	'$(HSC2HS)' $(HSC2HS_OPTS) --cross-compile $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: T3837
T3837:
	LANG=C '$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: T4340
T4340:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: T10272
T10272:
	'$(HSC2HS)' $(HSC2HS_OPTS) --cross-compile $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make $@
	./$@

.PHONY: T11004
T11004:
	LANG=C '$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.hs

.PHONY: T12504
T12504:
	'$(HSC2HS)' $(HSC2HS_OPTS) T12504/path/to/$@.hsc
ifeq "$(WINDOWS)" "YES"
	grep '{-# LINE 1 \"T12504\\\\path\\\\to\\\\$@\.hsc\" #-}' T12504/path/to/$@.hs
else
	grep '{-# LINE 1 \"T12504/path/to/$@\.hsc\" #-}' T12504/path/to/$@.hs
endif

.PHONY: T15758
T15758:
	'$(HSC2HS)' $(HSC2HS_OPTS) $@.hsc @T15758.response
	'$(TEST_HC)' $(TEST_HC_OPTS) -c $@.a.hs
