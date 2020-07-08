package com.github.codingdebugallday.driver.datasource.postgresql.common.infra.utils;

import java.net.*;
import java.util.Enumeration;
import java.util.List;

/**
 * <p>
 * IP工具类
 * </p>
 *
 * @author isaac 2020/6/29 14:22
 * @since 1.0
 */
public final class IpUtil {

    public static final String LOCAL_IP = getLocalIp();

    private IpUtil() {
        throw new IllegalStateException("Utils");
    }

    /**
     * 直接根据第一个网卡地址作为其内网ipv4地址，避免返回 127.0.0.1
     *
     * @return 本地IP
     */
    private static String getLocalIp() {
        try {
            Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
            while (networkInterfaces.hasMoreElements()) {
                NetworkInterface networkInterface = networkInterfaces.nextElement();
                if (networkInterface.isLoopback() || !networkInterface.isUp()) {
                    continue;
                }
                List<InterfaceAddress> interfaceAddresses = networkInterface.getInterfaceAddresses();
                for (InterfaceAddress interfaceAddress : interfaceAddresses) {
                    InetAddress address = interfaceAddress.getAddress();
                    if (interfaceAddress.getAddress() instanceof Inet4Address) {
                        Inet4Address inet4Address = (Inet4Address) address;
                        return inet4Address.getHostAddress();
                    }
                }
            }
            return InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException | SocketException e) {
            throw new IllegalStateException(e);
        }
    }

}
