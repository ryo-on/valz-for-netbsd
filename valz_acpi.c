/*	$NetBSD: vald_acpi.c,v 1.4 2010/04/15 07:02:24 jruoho Exp $ */

/*-
 * Copyright (c) 2002 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Masanori Kanaoka.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Copyright 2001 Bill Sommerfeld.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed for the NetBSD Project by
 *	Wasabi Systems, Inc.
 * 4. The name of Wasabi Systems, Inc. may not be used to endorse
 *    or promote products derived from this software without specific prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY WASABI SYSTEMS, INC. ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL WASABI SYSTEMS, INC
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * ACPI VALZ Driver for Toshiba dynabook R63/PS
 *	This driver is based on vald_acpi.c
 */

/*
 * Obtain information of Toshiba "GHCI" Method from next URL.
 *           http://www.buzzard.me.uk/toshiba/docs.html
 *           http://memebeam.org/toys/ToshibaAcpiDriver
 */

#include <sys/cdefs.h>
__KERNEL_RCSID(0, "$NetBSD: vald_acpi.c,v 1.4 2010/04/15 07:02:24 jruoho Exp $");

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/device.h>

#include <dev/acpi/acpica.h>
#include <dev/acpi/acpireg.h>
#include <dev/acpi/acpivar.h>

#define _COMPONENT		ACPI_RESOURCE_COMPONENT
ACPI_MODULE_NAME		("valz_acpi")

#define METHOD_HCI		"GHCI"
#define METHOD_HCI_ENABLE	"ENAB"

/* Operations */
/* Get */
#define HCI_GET			0xfe00
#define SCI_CHECK		0xf000
#define SCI_GET			0xf300

/* Set */
#define HCI_SET			0xff00
#define SCI_OPEN		0xf100
#define SCI_CLOSE		0xf200
#define SCI_SET			0xf400

/* Return codes */
#define HCI_SUCCESS		0x0000
#define HCI_FAILURE		0x1000
#define HCI_NOT_SUPPORTED	0x8000
#define HCI_FIFO_EMPTY		0x8c00

#define SCI_OPENCLOSE_OK	0x0044
#define SCI_NOT_SUPPORTED	0x8000
#define SCI_ALREADY_OPEN	0x8100
#define SCI_NOT_OPEN		0x8200
#define SCI_NOT_PRESENT		0x8600

/* Functions */
#define HCI_LCD_BACKLIGHT	0x0002
#define HCI_ACADAPTOR		0x0003
#define HCI_SYSTEM_EVENT_FIFO	0x0016
#define HCI_KBD_BACKLIGHT	0x0017
#define HCI_DISPLAY_DEV		0x001c
#define HCI_HOTKEY_EVENT	0x001e
#define HCI_LCD_BRIGHTNESS	0x002a
#define HCI_CPU_SPEED		0x0032

#define SCI_USB_OFF_CHARGE	0x0150
#define SCI_TOUCHPAD		0x050e
#define SCI_KBD_BACKLIGHT_STS	0x015c
#define SCI_KBD_BACKLIGHT	0x0095

#define SCI_KBD_BL_TIME_SHIFT	0x10

/* Field definitions */
#define HCI_LCD_BRIGHTNESS_BITS	3
#define HCI_LCD_BRIGHTNESS_SFT	(16 - HCI_LCD_BRIGHTNESS_BITS)
#define HCI_LCD_BRIGHTNESS_MAX	((1 << HCI_LCD_BRIGHTNESS_BITS) - 1)
#define HCI_VIDEO_DEVICE_FLG	0x0100
#define HCI_CPU_SPEED_BITS	3
#define HCI_CPU_SPEED_SFT	(16 - HCI_CPU_SPEED_BITS)
#define HCI_CPU_SPEED_MAX	((1 << HCI_CPU_SPEED_BITS) - 1)

/* Key press/release events */

/* Key press/release events */
#define FN_RELEASE_OFFSET	0x80
#  if 0
/* Not used */
#define FN_PRESS		0x01ff
#define FN_RELEASE		0x0100
#  endif
#define FN_ESC_PRESS		0x0101
#define FN_ESC_RELEASE		(FN_ESC_PRESS + FN_RELEASE_OFFSET)
#define FN_F1_PRESS		0x013b
#define FN_F1_RELEASE		(FN_F1_PRESS + FN_RELEASE_OFFSET)
#define FN_F2_PRESS		0x013c
#define FN_F2_RELEASE		(FN_F2_PRESS + FN_RELEASE_OFFSET)
#define FN_F3_PRESS		0x013d
#define FN_F3_RELEASE		(FN_F3_PRESS + FN_RELEASE_OFFSET)
#define FN_F4_PRESS		0x013e
#define FN_F4_RELEASE		(FN_F4_PRESS + FN_RELEASE_OFFSET)
#define FN_F5_PRESS		0x013f
#define FN_F5_RELEASE		(FN_F5_PRESS + FN_RELEASE_OFFSET)
#define FN_F6_PRESS		0x0140
#define FN_F6_RELEASE		(FN_F6_PRESS + FN_RELEASE_OFFSET)
#define FN_F7_PRESS		0x0141
#define FN_F7_RELEASE		(FN_F7_PRESS + FN_RELEASE_OFFSET)
#define FN_F8_PRESS		0x0142
#define FN_F8_RELEASE		(FN_F8_PRESS + FN_RELEASE_OFFSET)
#define FN_F9_PRESS		0x0143
#define FN_F9_RELEASE		(FN_F9_PRESS + FN_RELEASE_OFFSET)
/* Toggle, they are controlled by hardware */
#define FN_F10_ON		0x1bb0
#define FN_F10_OFF		0x1bb1
#define FN_F11_ON		0x1bb2
#define FN_F11_OFF		0x1bb3
/* Fn+F12 does not emit keycode */
/* dynabook R63/PS does not have KANJI keytop print */
#define FN_KNJ_PRESS		0x0129
#define FN_KNJ_RELEASE		(FN_KNJ_PRESS + FN_RELEASE_OFFSET)
#define FN_1_PRESS		0x0102
#define FN_1_RELEASE		(FN_1_PRESS + FN_RELEASE_OFFSET)
#define FN_2_PRESS		0x0103
#define FN_2_RELEASE		(FN_2_PRESS + FN_RELEASE_OFFSET)
/* Fn+3 and Fn+4 do not emit keybode */
#define FN_Z_PRESS		0x012c
#define FN_Z_RELEASE		(FN_1_PRESS + FN_RELEASE_OFFSET)
#define FN_SPACE_PRESS		0x0139
#define FN_SPACE_RELEASE	(FN_1_PRESS + FN_RELEASE_OFFSET)
#define FN_TAB_PRESS		0x010f
#define FN_TAB_RELEASE		(FN_TAB_PRESS + FN_RELEASE_OFFSET)

/* HCI register definitions */
#define HCI_WORDS		6 /* number of registers */
#define HCI_REG_AX		0 /* Operation -> return value */
#define HCI_REG_BX		1 /* Function */
#define HCI_REG_CX		2 /* Argument (in or out) */
#define HCI_REG_DX		2 /* unused */
#define HCI_REG_SI		3 /* unused */
#define HCI_REG_DI		4 /* unused */

#define HCI_ON			0x0001
#define HCI_OFF			0x0000
#define HCI_ENABLE		0x0001
#define HCI_DISABLE		0x0000

#define HCI_LCD			0x1
#define HCI_CRT			0x2
#define HCI_TV			0x4

struct valz_acpi_softc {
	device_t sc_dev;		/* base device glue */
	struct acpi_devnode *sc_node;	/* our ACPI devnode */
	int sc_flags;			/* see below */

	ACPI_HANDLE lcd_handle;		/* lcd handle */
	int *lcd_level;			/* lcd brightness table */
	int lcd_num;			/* size of lcd brightness table */
	int lcd_index;			/* index of lcd brightness table */

	ACPI_INTEGER sc_ac_status;	/* AC adaptor status when attach */
};

static const char * const valz_acpi_hids[] = {
	"TOS6208",
	NULL
};

static int	valz_acpi_match(device_t, cfdata_t, void *);
static void	valz_acpi_attach(device_t, device_t, void *);

static void	valz_acpi_event(void *);
static void	valz_acpi_notify_handler(ACPI_HANDLE, uint32_t, void *);

#define ACPI_NOTIFY_ValzHotkeyPressed	0x80
#define ACPI_NOTIFY_ValzLidClosed	0x8f

/* HCI manipulation */
static ACPI_STATUS	hci_op(struct valz_acpi_softc *,
				uint32_t *, uint32_t *);
static ACPI_STATUS	valz_acpi_hci_get(struct valz_acpi_softc *, uint32_t,
					uint32_t, uint32_t *, uint32_t *);
static ACPI_STATUS	valz_acpi_hci_set(struct valz_acpi_softc *, uint32_t,
					uint32_t, uint32_t, uint32_t *);

static ACPI_STATUS	sci_open(struct valz_acpi_softc *);
static ACPI_STATUS	sci_close(struct valz_acpi_softc *);

static ACPI_STATUS	valz_acpi_touchpad_toggle(struct valz_acpi_softc *);
static ACPI_STATUS	valz_acpi_lcd_backlight_toggle(
					struct valz_acpi_softc *sc);

CFATTACH_DECL_NEW(valz_acpi, sizeof(struct valz_acpi_softc),
    valz_acpi_match, valz_acpi_attach, NULL, NULL);

/*
 * valz_acpi_match:
 *
 *	Autoconfiguration `match' routine.
 */
static int
valz_acpi_match(device_t parent, cfdata_t match, void *aux)
{
	struct acpi_attach_args *aa = aux;

	if (aa->aa_node->ad_type != ACPI_TYPE_DEVICE)
		return (0);

	return (acpi_match_hid(aa->aa_node->ad_devinfo, valz_acpi_hids));
}

/*
 * valz_acpi_attach:
 *
 *	Autoconfiguration `attach' routine.
 */
static void
valz_acpi_attach(device_t parent, device_t self, void *aux)
{
	struct valz_acpi_softc *sc = device_private(self);
	struct acpi_attach_args *aa = aux;
	ACPI_STATUS rv;
	uint32_t value, result;

	aprint_naive(": Toshiba VALZ\n");
	aprint_normal(": Toshiba VALZ\n");

	sc->sc_node = aa->aa_node;
	sc->sc_dev = self;

	/* Get AC adaptor status via _PSR. */
	rv = acpi_eval_integer(ACPI_ROOT_OBJECT, "\\_SB_.ADP1._PSR",
	    &sc->sc_ac_status);
	if (ACPI_FAILURE(rv))
		aprint_error_dev(self, "Unable to evaluate _PSR: %s\n",
		    AcpiFormatException(rv));
	else
		aprint_verbose_dev(self, "AC adaptor %sconnected\n",
		    (sc->sc_ac_status == 0 ? "not ": ""));

	/* Get LCD backlight status. */
	rv = valz_acpi_hci_get(sc, HCI_GET, HCI_LCD_BACKLIGHT, &value, &result);
	if (ACPI_SUCCESS(rv)) {
		if (result != 0)
			aprint_error_dev(self, "can't get backlight status error result: %x, value: %x\n",
			    result, value);
		else
			aprint_verbose_dev(self, "LCD backlight %s\n",
			    ((value == HCI_ON) ? "on" : "off"));
	}

	/* Enable SystemEventFIFO,HotkeyEvent */
	rv = valz_acpi_hci_set(sc, HCI_SET, HCI_SYSTEM_EVENT_FIFO, HCI_ENABLE,
	    &result);
	if (ACPI_SUCCESS(rv) && result != 0)
		aprint_error_dev(self, "can't enable SystemEventFIFO error=%d\n",
		    result);

	rv = valz_acpi_hci_set(sc, HCI_SET, HCI_HOTKEY_EVENT, HCI_ENABLE, &result);
	if (ACPI_SUCCESS(rv) && result != 0)
		aprint_error_dev(self, "can't enable HotkeyEvent error=%d\n",
		    result);

	/* Check SystemFIFO events. */
	valz_acpi_event(sc);

	/* Get LCD brightness level via _BCL. */

	/* Set LCD brightness level via _BCM. */

	/* enable valz notify */
	rv = AcpiEvaluateObject(sc->sc_node->ad_handle, "ENAB", NULL, NULL);

	if (ACPI_SUCCESS(rv))
		(void)acpi_register_notify(sc->sc_node,
		    valz_acpi_notify_handler);
}

/*
 * valz_acpi_notify_handler:
 *
 *	Notify handler.
 */
static void
valz_acpi_notify_handler(ACPI_HANDLE handle, uint32_t notify, void *context)
{
	struct valz_acpi_softc *sc;
	device_t self = context;

	sc = device_private(self);

	switch (notify) {

	case ACPI_NOTIFY_ValzHotkeyPressed:
		(void)AcpiOsExecute(OSL_NOTIFY_HANDLER, valz_acpi_event, sc);
		break;

	case ACPI_NOTIFY_ValzLidClosed:
		/* Lid closed */
		break;

	default:
		aprint_error_dev(sc->sc_dev,
		    "unknown notify 0x%02X\n", notify);
		break;
	}
}

/*
 * valz_acpi_event:
 *
 *	Check hotkey event and do it, if event occur.
 */
static void
valz_acpi_event(void *arg)
{
	struct valz_acpi_softc *sc = arg;
	ACPI_STATUS rv;
	uint32_t value, result;

	while(1) {
		rv = valz_acpi_hci_get(sc, HCI_GET, HCI_SYSTEM_EVENT_FIFO,
			&value, &result);
		if (ACPI_SUCCESS(rv) && result == 0) {

			switch (value) {
			case FN_ESC_PRESS:
				/* Mute speaker */
				aprint_normal("Fn+ESC\n");
				break;
			case FN_F1_PRESS:
				/* Instant security */
				aprint_normal("Fn+F1\n");
				break;
			case FN_F2_PRESS:
				/* Toggle power plan */
				aprint_normal("Fn+F2\n");
				break;
			case FN_F3_PRESS:
				/* Sleep */
				aprint_normal("Fn+F3\n");
				break;
			case FN_F4_PRESS:
				/* Hibernate */
				break;
			case FN_F5_PRESS:
				/* Toggle external/internal display */
				break;
			case FN_F6_PRESS:
				/* Brightness down */
				aprint_normal("Fn+F6\n");
				break;
			case FN_F7_PRESS:
				/* Brightness up */
				aprint_normal("Fn+F7\n");
				break;
			case FN_F8_PRESS:
				/* Toggle WiFi and Bluetooth */
				break;
			case FN_F9_PRESS:
				/* Toggle touchpad */
				valz_acpi_touchpad_toggle(sc);
				break;
			case FN_F10_ON:
				/* Enable arrow key mode */
				break;
			case FN_F10_OFF:
				/* Disable arrow key mode */
				break;
			case FN_F11_ON:
				/* Numlock on */
				break;
			case FN_F11_OFF:
				/* Numlock off */
				break;
			case FN_KNJ_PRESS:
				/* undefined */
				break;
			case FN_1_PRESS:
				/* Zoom in */
				break;
			case FN_2_PRESS:
				/* Zoom out */
				break;
			case FN_Z_PRESS:
				/* Keyboard backlight */
				break;
			case FN_SPACE_PRESS:
				/* Change display resolution */
				break;
			case FN_TAB_PRESS:
				/* undefined */
				valz_acpi_lcd_backlight_toggle(sc);
				break;

			default:
				break;
			}
		}
		if (ACPI_FAILURE(rv) || result == HCI_NOT_SUPPORTED ||
			result == HCI_FIFO_EMPTY)
			break;
	}
}

/*
 * HCI/SCI operation
 */
static ACPI_STATUS
hci_op(struct valz_acpi_softc *sc, uint32_t *input,
			uint32_t *output)
{
	ACPI_STATUS rv;
	ACPI_OBJECT Arg[HCI_WORDS];
	ACPI_OBJECT_LIST ArgList;
	ACPI_OBJECT *param, *PrtElement;
	ACPI_BUFFER buf;
	int		i;

	for (i = 0; i < HCI_WORDS; i++) {
		Arg[i].Type = ACPI_TYPE_INTEGER;
		Arg[i].Integer.Value = 0;
	}

	for (i = 0; i < HCI_WORDS; i++) {
		Arg[i].Integer.Value = input[i];
	}

	ArgList.Count = HCI_WORDS;
	ArgList.Pointer = Arg;

	buf.Pointer = NULL;
	buf.Length = ACPI_ALLOCATE_LOCAL_BUFFER;

	rv = AcpiEvaluateObject(sc->sc_node->ad_handle,
	    METHOD_HCI, &ArgList, &buf);
	if (ACPI_FAILURE(rv)) {
		aprint_error_dev(sc->sc_dev, "failed to evaluate GHCI: %s\n",
		    AcpiFormatException(rv));
		return (rv);
	}

	param = buf.Pointer;
	if (param->Type == ACPI_TYPE_PACKAGE) {
		PrtElement = param->Package.Elements;
		for (i = 0; i < HCI_WORDS; i++) {
			output[i] = PrtElement->Integer.Value;
			PrtElement++;
		}
	}

	if (buf.Pointer)
		ACPI_FREE(buf.Pointer);
	return rv;
}

/*
 * valz_acpi_hci_get:
 *
 *	Get value via "GHCI" Method.
 */
static ACPI_STATUS
valz_acpi_hci_get(struct valz_acpi_softc *sc, uint32_t function,
    uint32_t reg, uint32_t *value, uint32_t *result)
{
	ACPI_STATUS rv;

	uint32_t input[HCI_WORDS];
	uint32_t output[HCI_WORDS];

	input[HCI_REG_AX] = function;
	input[HCI_REG_BX] = reg;
	input[HCI_REG_CX] = 0;

	rv = hci_op(sc, input, output);

	*result = output[HCI_REG_AX];
	*value = output[HCI_REG_CX];

	return rv;
}

/*
 * valz_acpi_hci_set:
 *
 *	Set value via "GHCI" Method.
 */
static ACPI_STATUS
valz_acpi_hci_set(struct valz_acpi_softc *sc, uint32_t function,
    uint32_t reg, uint32_t value, uint32_t *result)
{
	ACPI_STATUS rv;

	uint32_t input[HCI_WORDS];
	uint32_t output[HCI_WORDS];

	input[HCI_REG_AX] = function;
	input[HCI_REG_BX] = reg;
	input[HCI_REG_CX] = value;

	rv = hci_op(sc, input, output);

	*result = output[HCI_REG_AX];

	return rv;
}

/*
 * Open SCI
 */
static ACPI_STATUS
sci_open(struct valz_acpi_softc *sc)
{
	ACPI_STATUS rv;
	uint32_t result;

	rv = valz_acpi_hci_set(sc, SCI_OPEN, 0, 0, &result);
	if (ACPI_FAILURE(rv)) {
		aprint_error("SCI: ACPI set error\n");
	} else {
		switch (result) {
		case SCI_OPENCLOSE_OK:
			aprint_debug("Opening SCI\n");
			break;
		case SCI_ALREADY_OPEN:
			aprint_error("SCI already open\n");
			break;
		case SCI_NOT_SUPPORTED:
			aprint_error("SCI is not supported\n");
			break;
		case SCI_NOT_PRESENT:
			aprint_error("SCI is not present\n");
			break;
		default:
			aprint_error("SCI: undefined behavior\n");
			break;
		}
	}

	return rv;
}

/*
 * Close SCI
 */
static ACPI_STATUS
sci_close(struct valz_acpi_softc *sc)
{
	ACPI_STATUS rv;
	uint32_t result;

	rv = valz_acpi_hci_set(sc, SCI_CLOSE, 0, 0, &result);
	if (ACPI_FAILURE(rv)) {
		aprint_error("SCI: ACPI set error\n");
	} else {
		switch (result) {
		case SCI_OPENCLOSE_OK:
			aprint_debug("Closing SCI\n");
			break;
		case SCI_NOT_OPEN:
			aprint_error("SCI is not opened\n");
			break;
		case SCI_NOT_SUPPORTED:
			aprint_error("SCI is not supported\n");
			break;
		case SCI_NOT_PRESENT:
			aprint_error("SCI is not present\n");
			break;
		default:
			aprint_error("SCI: undefined behavior\n");
			break;
		}
	}

	return rv;
}

/*
 * Enable/disable touchpad and trackpoint with HCI_ENABLE/HCI_DISABLE
 */
static ACPI_STATUS
valz_acpi_touchpad_toggle(struct valz_acpi_softc *sc)
{
	ACPI_STATUS rv;
	uint32_t result, status, value;

	rv = sci_open(sc);
	if (ACPI_FAILURE(rv))
		aprint_error_dev(sc->sc_dev,
				"Cannot open SCI: %s\n",
				AcpiFormatException(rv));

	rv = valz_acpi_hci_get(sc, SCI_GET, SCI_TOUCHPAD, &value, &result);
	if (ACPI_FAILURE(rv))
		aprint_error_dev(sc->sc_dev,
				"Cannot get SCI touchpad status: %s\n",
				AcpiFormatException(rv));

	switch (value) {
	case HCI_ENABLE:
		status = HCI_DISABLE;
		break;
	case HCI_DISABLE:
		status = HCI_ENABLE;
		break;
	default:
		status = HCI_ENABLE;
		break;
	}

	rv = valz_acpi_hci_set(sc, SCI_SET, SCI_TOUCHPAD, status, &result);
	if (ACPI_FAILURE(rv))
		aprint_error_dev(sc->sc_dev,
				"Cannot set SCI touchpad status: %s\n",
				AcpiFormatException(rv));

	rv = sci_close(sc);
	if (ACPI_FAILURE(rv))
		aprint_error_dev(sc->sc_dev,
				"Cannot close SCI: %s\n",
				AcpiFormatException(rv));

	return rv;
}

/*
 * Enable/disable LCD backlight with HCI_ENABLE/HCI_DISABLE
 */
static ACPI_STATUS
valz_acpi_lcd_backlight_toggle(struct valz_acpi_softc *sc)
{
	ACPI_STATUS rv;
	uint32_t result, status, value;

	rv = valz_acpi_hci_get(sc, HCI_GET, HCI_LCD_BACKLIGHT, &value, &result);
	if (ACPI_FAILURE(rv))
		aprint_error_dev(sc->sc_dev,
				"Cannot get HCI LCD backlight status: %s\n",
				AcpiFormatException(rv));

	switch (value) {
	case HCI_ON:
		status = HCI_OFF;
		break;
	case HCI_OFF:
		status = HCI_ON;
		break;
	default:
		status = HCI_ON;
		break;
	}

	rv = valz_acpi_hci_set(sc, HCI_SET, HCI_LCD_BACKLIGHT, status, &result);
	if (ACPI_FAILURE(rv))
		aprint_error_dev(sc->sc_dev,
				"Cannot set HCI LCD backlight status: %s\n",
				AcpiFormatException(rv));

	return rv;
}

